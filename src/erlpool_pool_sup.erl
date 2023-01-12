-module(erlpool_pool_sup).

-include("erlpool.hrl").

-behaviour(supervisor).

-define(SUPERVISOR_NAME(PoolName), list_to_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(PoolName) ++ "_sup")).

-export([
    start_link/2,
    init/1,
    start_worker/3,
    add_worker/3,
    name/1
]).

start_link(PoolName, PoolArgs) ->
    supervisor:start_link({local, ?SUPERVISOR_NAME(PoolName)}, ?MODULE, [PoolName, PoolArgs]).

name(PoolName) ->
    ?SUPERVISOR_NAME(PoolName).

%internals

init([PoolName, PoolArgs]) ->
    PoolSize = proplists:get_value(size, PoolArgs),
    SupRestartPeriod = proplists:get_value(supervisor_period, PoolArgs, ?DEFAULT_MAX_PERIOD_SEC),
    SupIntensity = proplists:get_value(supervisor_intensity, PoolArgs, ?DEFAULT_MAX_INTENSITY),
    SupRestartStrategy = proplists:get_value(supervisor_restart, PoolArgs, ?DEFAULT_SUP_RESTART_STRATEGY),
    SupShutdown = proplists:get_value(supervisor_shutdown, PoolArgs, ?DEFAULT_SUP_SHUTDOWN),

    MFA = proplists:get_value(start_mfa, PoolArgs),

    PoolTable = ets:new(PoolName, [named_table, public, set, {write_concurrency, true}, {read_concurrency, true}]),
    true = ets:insert(PoolTable, [{sq, 0}]),

    CreateFun = fun(Id) ->
        children_specs(Id, SupRestartStrategy, SupShutdown, [Id, PoolTable, MFA])
    end,

    Ch = lists:map(CreateFun, lists:seq(1, PoolSize)),

    {ok, {{one_for_one, SupIntensity, SupRestartPeriod}, Ch}}.

start_worker(Id, PoolTable, {M, F, A}) ->
    {ok, Pid} = erlang:apply(M, F, A),
    true = ets:insert(PoolTable, {Id, Pid}),
    {ok, Pid}.

add_worker(PoolName, Id, PoolArgs) ->
    SupName = name(PoolName),
    SupRestartStrategy = proplists:get_value(supervisor_restart, PoolArgs, ?DEFAULT_SUP_RESTART_STRATEGY),
    SupShutdown = proplists:get_value(supervisor_shutdown, PoolArgs, ?DEFAULT_SUP_SHUTDOWN),
    MFA = proplists:get_value(start_mfa, PoolArgs),
    ChildSpec = children_specs(Id, SupRestartStrategy, SupShutdown, [Id, PoolName, MFA]),
    supervisor:start_child(SupName, ChildSpec).

children_specs(Id, SupRestartStrategy, SupShutdown, Args) ->
    {Id, {?MODULE, start_worker, Args}, SupRestartStrategy, SupShutdown, worker, [?MODULE]}.
