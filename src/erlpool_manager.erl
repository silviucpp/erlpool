-module(erlpool_manager).

-define(POOL_MANAGER_TAB, erlpool_manager).

-export([
    init/0,
    new_pool/2,
    rem_pool/1,
    rem_group/1,
    get_pools/1
]).

init() ->
    ?POOL_MANAGER_TAB = ets:new(?POOL_MANAGER_TAB, [named_table, public, {read_concurrency, true}]),
    ok.

new_pool(PoolName, PoolArgs) ->
    case erlpool_sup:add_pool(PoolName, PoolArgs) of
        {ok, _} ->
            PoolSize  = proplists:get_value(size, PoolArgs),
            ets:insert(?POOL_MANAGER_TAB, {PoolName, PoolSize, PoolArgs}),
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

rem_group(Group) ->
    try
        RemoveFun = fun({PoolName, _Size, PoolArgs}) ->
            case proplists:get_value(group, PoolArgs) of
                Group ->
                    ok = rem_pool(PoolName);
                _ ->
                    ok
            end
        end,
        lists:foreach(RemoveFun, ets:tab2list(?POOL_MANAGER_TAB))
    catch
        _: Error ->
            {error, Error}
    end.

get_pools(Group) ->
    try
        GetFun = fun({PoolName, _Size, PoolArgs}, Acc) ->
            case proplists:get_value(group, PoolArgs) of
                Group ->
                    [PoolName|Acc];
                _ ->
                    Acc
            end
        end,
        {ok, lists:foldl(GetFun, [], ets:tab2list(?POOL_MANAGER_TAB))}
    catch
        _: Error ->
            {error, Error}
    end.
