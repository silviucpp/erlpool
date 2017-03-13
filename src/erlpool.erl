-module(erlpool).

-author("silviu.caragea").

-include("erlpool.hrl").

-define(POOL_SIZE(PoolName), erlpool_globals:PoolName()).

-export([
    start/0,
    start/1,
    stop/0,
    start_pool/2,
    stop_pool/1,
    pid/1,
    map/2,
    pool_size/1
]).

-spec start() -> ok  | {error, any()}.

start() ->
    start(temporary).

-spec start(permanent | transient | temporary) -> ok | {error, any()}.

start(Type) ->
    case application:ensure_all_started(erlpool, Type) of
        {ok, _} ->
            ok;
        Other ->
            Other
    end.

-spec stop() -> ok.

stop() ->
    application:stop(erlpool).

-spec start_pool(atom(), [pool_option()]) -> ok | {error, any()}.

start_pool(PoolName, PoolArgs) ->
    erlpool_manager:new_pool(PoolName, PoolArgs).

-spec stop_pool(atom()) -> ok | {error, any()}.

stop_pool(PoolName) ->
    erlpool_manager:rem_pool(PoolName).

-spec pid(atom()) -> pid() | {error, any()}.

pid(PoolName) ->
    try
        N = ets:update_counter(PoolName, sq, {2, 1, ?POOL_SIZE(PoolName), 1}),
        [{N, Worker}] = ets:lookup(PoolName, N),
        Worker
    catch
        _:Error ->
            {error, Error}
    end.

-spec map(atom(), fun()) -> [term()] | {error, any()}.

map(PoolName, Fun) ->
    try
        FunFoldl = fun({Id, Pid}, Acc) ->
            case is_integer(Id) of
                true ->
                    [Pid|Acc];
                _ ->
                    Acc
            end
        end,
        lists:map(Fun, ets:foldl(FunFoldl, [], PoolName))
    catch
        _:Error ->
            {error, Error}
    end.

-spec pool_size(atom()) -> non_neg_integer() | {error, any()}.

pool_size(PoolName) ->
    try
        ?POOL_SIZE(PoolName)
    catch
        _:Error ->
            {error, Error}
    end.