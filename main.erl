-module(main).
-export([start/0, stop/0, allocate/0, deallocate/1, init/0]).

start() ->
    register(clusters, spawn(?MODULE, init, [])).

init() ->
    Clusters = {get_clusters(), []}, % {Free, Allocated}
    loop(Clusters).

get_clusters() -> [c1, c2, c3, c4, c5].

loop(Clusters) ->
    receive
        {request, Pid, allocate} ->
            {NewClusters, Reply} = allocate(Clusters, Pid),
            reply(Pid, Reply),
            loop(NewClusters);
        {request, Pid , {deallocate, ClusterID}} ->
            NewClusters = deallocate(Clusters, ClusterID),
            reply(Pid, ok),
            loop(NewClusters);
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