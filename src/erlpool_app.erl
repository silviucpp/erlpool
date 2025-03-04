-module(erlpool_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    ok = erlpool_compile:compile_settings([]),
    ok = erlpool_manager:init(),
    {ok, Pid} = erlpool_sup:start_link(),
    start_pools(),
    {ok, Pid}.

stop(_State) ->
    ok.

start_pools() ->
    FunPool = fun({Name, Args}) ->
        ok = erlpool:start_pool(Name, Args)
    end,
    lists:foreach(FunPool, get_pools()).

get_pools() ->
    case erlpool_utils:env(pools) of
        undefined ->
            [];
        Value ->
            Value
    end.


