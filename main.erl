-module(main).
-export([start/0, stop/0, allocate/0, deallocate/1, initConveyors/1]).

start() ->
    {ok, belts} = io:read("Number of Belts"),
    {ok, size} = io:read("Max truck size"),
    register(conveyors, spawn(?MODULE, initConveyors(belts))),
    register(trucks, spawn(?MODULE, initTrucks(belts))).

get_List(N) -> get_List(N, []).
get_List(0, clusters) -> lists:reverse(clusters);
get_List(N, clusters) when N > 0 -> get_List(N - 1 , [N, clusters]).

initConveyors(belts) ->     
    Conveyors = {get_List(belts), []},
    loopConveyors(Conveyors).

initTrucks(belts) ->     
    Trucks = {get_List(belts), []},
    loopTrucks(Trucks).
    
loopConveyors(Conveyors) ->
    receive
        {request, Pid, allocate} ->
            {NewConveyors, Reply} = allocate(Conveyors, Pid),
            reply(Pid, Reply),
            loopConveyors(NewConveyors);
        {request, Pid , {deallocate, ConveyorID}} ->
            NewConveyors = deallocate(Conveyors, ConveyorID),
            reply(Pid, ok),
            loopConveyors(NewConveyors);
    {request, Pid, stop} ->
            reply(Pid, ok)
    end.

loopTrucks(Trucks) ->
    receive
        {request, Pid, allocate} ->
            {NewTrucks, Reply} = allocate(Trucks, Pid),
            reply(Pid, Reply),
            loopTrucks(NewTrucks);
        {request, Pid , {deallocate, TruckID}} ->
            NewTrucks = deallocate(Trucks, TruckID),
            reply(Pid, ok),
            loopTrucks(NewTrucks);
    {request, Pid, stop} ->
            reply(Pid, ok)
    end.

reply(Pid, Reply) -> Pid ! {reply, Reply}.


stop() -> call(stop).
allocate() -> call(allocate).
deallocate(Cluster) -> call({deallocate, Cluster}).
call(Message) ->
    clusters ! {request, self(), Message},
    receive
        {reply, Reply} -> Reply
    end.


allocate({[], Allocated}, _) ->
    {{[], Allocated}, % NewClusters
    {error, no_cluster}}; % Reply
allocate({[ClusterID|Free], Allocated}, Pid) ->
    {{Free, [{ClusterID, Pid}|Allocated]}, % NewClusters
    {ok, ClusterID}}. % Reply
deallocate({Free, Allocated}, ClusterID) ->
    NewAllocated = lists:keydelete(ClusterID, 1, Allocated),
    {[ClusterID|Free], NewAllocated}.