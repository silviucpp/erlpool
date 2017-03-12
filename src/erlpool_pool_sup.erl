-module(erlpool_pool_sup).

-author("silviu.caragea").

-include("erlpool.hrl").

-behaviour(supervisor).

-define(SUPERVISOR_NAME(PoolName), list_to_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(PoolName) ++ "_sup")).

-export([
    start_link/2,
    init/1,
    start_worker/3
]).

start_link(PoolName, PoolArgs) ->
    supervisor:start_link({local, ?SUPERVISOR_NAME(PoolName)}, ?MODULE, [PoolName, PoolArgs]).

%internals

init([PoolName, PoolArgs]) ->
    PoolSize = proplists:get_value(size, PoolArgs),
    SupRestartPeriod = proplists:get_value(supervisor_period, PoolArgs, ?DEFAULT_MAX_PERIOD_SEC),
    SupIntensity = proplists:get_value(supervisor_intensity, PoolArgs, ?DEFAULT_MAX_INTENSITY),
    {M, _, _} = MFA = proplists:get_value(start_mfa, PoolArgs),

    PoolTable = ets:new(PoolName, [named_table, public, set, {write_concurrency, true}, {read_concurrency, true}]),
    true = ets:insert(PoolTable, [{sq, 0}]),

    CreateFun = fun(Id) ->
        children_specs(Id, M, [Id, PoolTable, MFA])
    end,

    Ch = lists:map(CreateFun, lists:seq(1, PoolSize)),

    {ok, {{one_for_one, SupIntensity, SupRestartPeriod}, Ch}}.

start_worker(Id, PoolTable, {M, F, A}) ->
    {ok, Pid} = erlang:apply(M, F, A),
    true = ets:insert(PoolTable, {Id, Pid}),
    {ok, Pid}.

children_specs(Name, _Module, Args) ->
    {Name, {?MODULE, start_worker, Args}, transient, 2000, worker, []}.
