-module(erlpool_manager).
-author("silviu.caragea").

-define(POOL_MANAGER_TAB, erlpool_manager).

-export([
    init/0,
    new_pool/2,
    rem_pool/1
]).

init() ->
    ?POOL_MANAGER_TAB = ets:new(?POOL_MANAGER_TAB, [named_table, public, {read_concurrency, true}]),
    ok.

new_pool(PoolName, PoolArgs) ->
    case erlpool_sup:add_pool(PoolName, PoolArgs) of
        {ok, _} ->
            PoolSize = proplists:get_value(size, PoolArgs),
            ets:insert(?POOL_MANAGER_TAB, {PoolName, PoolSize}),
            erlpool_compile:compile_settings(ets:tab2list(?POOL_MANAGER_TAB));
        Error ->
            Error
    end.

rem_pool(PoolName) ->
    case erlpool_sup:remove_pool(PoolName) of
        ok ->
            ets:delete(?POOL_MANAGER_TAB, PoolName),
            erlpool_compile:compile_settings(ets:tab2list(?POOL_MANAGER_TAB));
        Error ->
            Error
    end.