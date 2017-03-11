-module(erlpool).

-author("silviu.caragea").

-include("erlpool.hrl").

-behaviour(supervisor).

-define(SUPERVISOR_NAME(PoolName), list_to_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(PoolName) ++ "_sup")).

-export([
    start_link/2,
    stop/1,
    pid/1,
    map/2,
    %internals
    init/1,
    start_worker/3
]).

-spec start_link(atom(), [pool_option()]) -> {ok, pid()}.

start_link(PoolName, PoolArgs) ->
    SupName = ?SUPERVISOR_NAME(PoolName),
    supervisor:start_link({local, SupName}, ?MODULE, [PoolName, PoolArgs]).

-spec stop(atom()) -> boolean().

stop(PoolName) ->
    case whereis(?SUPERVISOR_NAME(PoolName)) of
        undefined ->
            true;
        Pid ->
            exit(Pid, shutdown)
    end.

-spec pid(atom()) -> pid().

pid(PoolName) ->
    PoolSize = erlpool_manager:pool_size(PoolName),
    N = ets:update_counter(PoolName, sq, {2, 1, PoolSize, 1}),
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

%internals

init([PoolName, PoolArgs]) ->
    PoolSize = proplists:get_value(size, PoolArgs),
    SupRestartPeriod = proplists:get_value(supervisor_period, PoolArgs, ?DEFAULT_MAX_PERIOD_SEC),
    SupIntensity = proplists:get_value(supervisor_intensity, PoolArgs, ?DEFAULT_MAX_INTENSITY),
    {M, _, _} = MFA = proplists:get_value(start_mfa, PoolArgs),

    PoolTable = ets:new(PoolName, [named_table, public, set, {write_concurrency, true}, {read_concurrency, true}]),

    true = erlpool_manager:register(PoolName, PoolSize),
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