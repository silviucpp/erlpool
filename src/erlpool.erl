-module(erlpool).

-author("silviu.caragea").

-include("erlpool.hrl").

-export([
    start_link/2,
    pid/1,
    map/2
]).

-spec start_link(atom(), [pool_option()]) -> {ok, pid()}.

start_link(PoolName, PoolArgs) ->
    erlpool_sup:start_link(PoolName, PoolArgs).

-spec pid(atom()) -> pid().

pid(PoolName) ->
    erlpool_sup:next_pid(PoolName).

-spec map(atom(), fun()) -> [term()].

map(PoolName, Fun) ->
    FunFoldl = fun({Id, Pid}, Acc) ->
        case is_integer(Id) of
            true ->
                [Pid|Acc];
            _ ->
                Acc
        end
    end,
    Pids = ets:foldl(FunFoldl, [], PoolName),
    lists:map(Fun, Pids).
