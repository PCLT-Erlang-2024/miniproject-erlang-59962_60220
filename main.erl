-module(main).
-export([start/0]).

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
        {request, Pid, send_package} ->
            allocate(Conveyors, Pid),
            reply(Pid, add_package()),
            loopConveyors(Conveyors);
        {request, Pid, stop} ->
            reply(Pid, ok)
    end.

loopTrucks(Trucks) ->
    receive
        {request, Pid, allocate} ->
            {NewTrucks, Reply} = allocate(Trucks, Pid),
            reply(Pid, Reply),
            loopTrucks(NewTrucks);
        {request, Pid, stop} ->
            reply(Pid, ok)
    end.

reply(Pid, Reply) -> Pid ! {reply, Reply}.


send_package() -> call_conveyor(send_package).
add_package() -> call_truck(add_package).
stop_conveyors() -> call_conveyor(stop).
stop_trucks() -> call_truck(stop).
call_conveyor(Message) ->
    conveyors ! {request, self(), Message},
    receive
        {reply, Reply} -> Reply
    end.

call_truck({Message, ClusterID}) ->
    trucks ! {request, self(), Message},
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

allocate_truck({Free, Allocated}, Pid, ClusterID) -> {
    {{lists:keydelete(ClusterID, 1, Free), [{ClusterID, Pid}|Allocated]}, ok}
}.