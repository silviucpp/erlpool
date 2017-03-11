-module(erlpool).

-author("silviu.caragea").

-include("erlpool.hrl").

-define(POOL_SIZE(PoolName), erlpool_globals:PoolName()).

-export([
    start_pool/2,
    stop_pool/1,
    pid/1,
    map/2,
    pool_size/1
]).

-spec start_pool(atom(), [pool_option()]) -> ok | {error, any()}.

start_pool(PoolName, PoolArgs) ->
    erlpool_manager:new_pool(PoolName, PoolArgs).

-spec stop_pool(atom()) -> ok | {error, any()}.

stop_pool(PoolName) ->
    erlpool_manager:rem_pool(PoolName).

-spec pid(atom()) -> pid().

pid(PoolName) ->
    N = ets:update_counter(PoolName, sq, {2, 1, ?POOL_SIZE(PoolName), 1}),
    [{N, Worker}] = ets:lookup(PoolName, N),
    Worker.

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

-spec pool_size(atom()) -> non_neg_integer().

pool_size(PoolName) ->
    ?POOL_SIZE(PoolName).