-module(erlpool_sup).

-include("erlpool.hrl").

-behaviour(supervisor).

-export([
    start_link/2,
    pool_size/1,
    next_pid/1,
    init/1,
    start_worker/3
]).

-compile({inline,[pool_size/1]}).

start_link(PoolName, PoolArgs) ->
    SupName = list_to_atom(atom_to_list(?MODULE) ++ atom_to_list(PoolName) ++ "_sup"),
    supervisor:start_link({local, SupName}, ?MODULE, [PoolName, PoolArgs]).

pool_size(PoolName) ->
    %mochiglobal:get(PoolName),
    [{pool_size, PoolSize}] = ets:lookup(PoolName, pool_size),
    PoolSize.

next_pid(PoolName) ->
    [{pool_size, PoolSize}] = ets:lookup(PoolName, pool_size),
    N = ets:update_counter(PoolName, sequence, {2, 1, PoolSize, 1}),
    [{N, Worker}] = ets:lookup(PoolName, N),
    Worker.

%internals

init([PoolName, PoolArgs]) ->
    PoolTable = ets:new(PoolName, [named_table, public, {write_concurrency, true}, {read_concurrency, true}]),

    {M, _, _} = MFA = proplists:get_value(start_mfa, PoolArgs),
    PoolSize = proplists:get_value(size, PoolArgs),
    SupRestartPeriod = proplists:get_value(supervisor_period, PoolArgs, ?DEFAULT_MAX_PERIOD_SEC),
    SupIntensity = proplists:get_value(supervisor_intensity, PoolArgs, ?DEFAULT_MAX_INTENSITY),

    %ok = mochiglobal:put(PoolName, PoolSize),
    true = ets:insert(PoolTable, [{pool_size, PoolSize}, {sequence, 0}]),

    CreateFun = fun(Id) ->
        children_specs(Id, M, [Id, PoolTable, MFA])
    end,

    Ch = lists:map(CreateFun, lists:seq(1, PoolSize)),

    {ok, {{one_for_one, SupIntensity, SupRestartPeriod}, Ch}}.

start_worker(Id, PoolTable, {M, F, A}) ->
    {ok, Pid} = erlang:apply(M, F, A),
    true = ets:insert(PoolTable, {Id, Pid}),
    {ok, Pid}.

children_specs(Name, Module, Args) ->
    {Name, {?MODULE, start_worker, Args}, transient, 2000, worker, [Module]}.

