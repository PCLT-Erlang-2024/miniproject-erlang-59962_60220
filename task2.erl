-module(task2).
-export([start/0, initConveyors/1, initTrucks/2, loopProducer/1]).
-export([reply/2, restart/2, send_package/1, add_package/3]).


start() ->
    InputBelts = string:trim(io:get_line("Number of Belts: ")),
    InputSize = string:trim(io:get_line("Max truck size: ")),
    InputPackSize = string:trim(io:get_line("Max Package size: ")),
    {Belts, _} = string:to_integer(InputBelts),
    {Size, _} = string:to_integer(InputSize),
    {PackSize, _} = string:to_integer(InputPackSize),
    register(conveyors, spawn(?MODULE, initConveyors, [Belts])),
    register(trucks, spawn(?MODULE, initTrucks, [Belts, Size])),
    register(producer, spawn(?MODULE, loopProducer, [PackSize])),
    io:format("Created producer in ~p~n", [whereis(producer)]).


get_List(N) when is_tuple(N)-> get_TupleList(N, []);
get_List(N) -> get_List(N, []).

get_List(N, Clusters) when N > 0 -> get_List(N - 1 , [{N} | Clusters]);
get_List(0, Clusters) -> Clusters.

get_TupleList({N, S}, Clusters) when N > 0 -> get_TupleList({N-1, S}, [{N, S} | Clusters]);
get_TupleList({0, _}, Clusters) -> Clusters.

initConveyors(Belts) when is_integer(Belts) ->     
    Conveyors = {get_List(Belts), []},
    io:format("Created conveyors in ~p~n", [whereis(conveyors)]),
    loopConveyors(Conveyors, Belts).

initTrucks(Belts, Size) when is_integer(Belts) and is_integer(Size) ->
    Trucks = {get_List({Belts, Size}), []},
    io:format("Created trucks in ~p~n", [whereis(trucks)]),
    loopTrucks(Trucks, Size).

%-----------------Package maker------------------------
loopProducer(PackSize) ->
    Size = rand:uniform(PackSize),
    timer:sleep(200),
    spawn(?MODULE, send_package, [Size]),
    loopProducer(PackSize).


send_package(Size) -> call_conveyor({send_package, Size}).

call_conveyor(Message) ->
    conveyors ! {request, self(), Message},
    receive
        {reply, Reply} -> Reply;
        {restart, Message} -> timer:sleep(200), call_conveyor(Message)
    end.

reply(Pid, Reply) -> Pid ! {reply, Reply}.
restart(Pid, Message) -> Pid ! {restart, Message}.

%--------------------Conveyor Loop----------------------
loopConveyors(Conveyors, ConvNum) ->
    receive
        {request, Pid, {send_package, Size}} ->
            {NewConveyors, {_, ConvID}} = getConveyor(Conveyors, ConvNum),
            io:format("Sending package to truck ~p~n", [ConvID]),
            add_package(Pid, ConvID, Size),
            UpdatedConveyors = deallocate(NewConveyors, ConvID)
    end,
    loopConveyors(UpdatedConveyors, ConvNum).

getConveyor(Conveyors, ConvNum) ->
    ConvID = rand:uniform(ConvNum),
    case allocate(Conveyors, ConvID) of
        {NewConveyors, {ok, ClusterID}} -> 
            {NewConveyors, {ok, ClusterID}};
        {_, {error, no_conveyor}} ->
            getConveyor(Conveyors, ConvNum)
    end.

add_package(Pid, ClusterID, Size) -> trucks ! {request, self(), {add_package , Pid, ClusterID, Size}}.
allocate({Free, Allocated}, ClusterID) ->
    case lists:keyfind(ClusterID, 1, Free) of
        {ClusterID} ->
            {
                {lists:keydelete(ClusterID, 1, Free), [{ClusterID} | Allocated]},
                {ok, ClusterID}
            };
        false ->
            {
                {Free, Allocated},
                {error, no_conveyor}
            }
    end.
deallocate({Free, Allocated}, ClusterID) ->
    NewAllocated = lists:keydelete(ClusterID, 1, Allocated),
    {[{ClusterID}|Free], NewAllocated}.

%---------------------Truck Loop----------------------
loopTrucks(Trucks, Max) ->
    receive
        {request, _, {add_package, Pid, ClusterID, Size}} ->
            io:format("Current Trucks: ~p~n", [Trucks]),
            case allocate_truck(Trucks, ClusterID) of
                {NewTrucks, {ok, {ClusterID, S}}} when S >= Size ->
                    S1 = S - Size,
                    S2 = if 
                        S1 == 0 -> Max;
                        true -> S1        
                    end,
                    UpdatedTrucks = deallocate_truck(NewTrucks, S2, ClusterID),
                    reply(Pid, io:format("Truck ~p loaded package with size ~p~n", [ClusterID, Size])),
                    loopTrucks(UpdatedTrucks, Max);
                {NewTrucks, {ok, {ClusterID, _}}} ->
                    restart(Pid, {send_package, Size}),
                    UpdatedTrucks = deallocate_truck(NewTrucks, Max, ClusterID),
                    loopTrucks(UpdatedTrucks, Max);
                {_, {error, no_cluster}} ->
                    restart(Pid, {send_package, Size}),
                    loopTrucks(Trucks, Max)
            end
    end.

allocate_truck({Free, Allocated}, ClusterID) ->
    case lists:keyfind(ClusterID, 1, Free) of
        {ClusterID, Size} ->
            {
                {lists:keydelete(ClusterID, 1, Free), [{ClusterID, Size} | Allocated]},
                {ok, {ClusterID, Size}}
            };
        false ->
            {
                {Free, Allocated},
                {error, no_truck}
            }
    end.

deallocate_truck({Free, Allocated}, NewSize, ClusterID) -> 
    NewAllocated = lists:keydelete(ClusterID, 1, Allocated), 
    {[{ClusterID, NewSize}|Free], NewAllocated}.