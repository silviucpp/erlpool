-module(erlpool).

-include("erlpool.hrl").

-define(POOL_SIZE(PoolName),
    erlpool_globals:size(PoolName)).

-export([
    start/0,
    start/1,
    stop/0,
    start_pool/2,
    stop_pool/1,
    restart_pool/1,
    stop_group/1,
    restart_group/1,
    pid/1,
    sticky_pid/2,
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

-spec restart_pool(atom()) -> boolean().

restart_pool(PoolName) ->
    case erlpool_sup:restart_pool(PoolName) of
        {ok, _} ->
            true;
        _ ->
            false
    end.

-spec stop_group(term()) -> ok | {error, any()}.

stop_group(GroupName) ->
    erlpool_manager:rem_group(GroupName).

-spec restart_group(term()) -> ok | {error, any()}.

restart_group(GroupName) ->
    case erlpool_manager:get_pools(GroupName) of
        {ok, Pools} ->
            lists:foreach(fun(P) -> restart_pool(P) end, Pools);
        Error ->
            Error
    end.

-spec pid(atom()) -> pid() | {error, any()}.

pid(PoolName) ->
    case ?POOL_SIZE(PoolName) of
        {ok, PoolSize} ->
            try
                N = ets:update_counter(PoolName, sq, {2, 1, PoolSize, 1}),
                [{N, Worker}] = ets:lookup(PoolName, N),
                Worker
            catch
                _:Error ->
                    {error, Error}
            end;
        Error ->
            Error
    end.

-spec sticky_pid(atom(), non_neg_integer()) -> pid() | {error, any()}.

sticky_pid(PoolName, KeyHash) ->
    case ?POOL_SIZE(PoolName) of
        {ok, PoolSize} ->
            try
                [{_, Worker}] = ets:lookup(PoolName, KeyHash rem PoolSize + 1),
                Worker
            catch
                _:Error ->
                    {error, Error}
            end;
        Error ->
            Error
    end.

-spec map(atom(), fun()) -> [term()] | {error, any()}.

map(PoolName, Fun) ->
    case ?POOL_SIZE(PoolName) of
        {ok, _PoolSize} ->
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
            end;
        Error ->
            Error
    end.

-spec pool_size(atom()) -> non_neg_integer() | {error, any()}.

pool_size(PoolName) ->
    case ?POOL_SIZE(PoolName) of
        {ok, Size} ->
            Size;
        Error ->
            Error
    end.
