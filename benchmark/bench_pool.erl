-module(bench_pool).
-author("silviu.caragea").

-export([start/1, bench/2]).

-define(POOLBOY, poolboy).
-define(POOLER, pooler).
-define(CUESPORT, cuesport).
-define(REVOLVER, revolver).
-define(ERLPOOL, erlpool).

-define(POOL_SIZE, 20).
-define(WORKER_MODULE, benchmark_worker).
-define(WORKER_FUN, start_link).
-define(WORKER_ARGS, []).

bench(Number, Concurrency) ->
    S1 = start(?ERLPOOL),
    S2 = start(?CUESPORT),
    S3 = start(?REVOLVER),
    S4 = start(?POOLBOY),
    S5 = start(?POOLER),

    bench(?ERLPOOL, Number, Concurrency),
    bench(?CUESPORT, Number, Concurrency),
    bench(?REVOLVER, Number, Concurrency),
    bench(?POOLBOY, Number, Concurrency),
    bench(?POOLER, Number, Concurrency),

    stop(?ERLPOOL, S1),
    stop(?CUESPORT, S2),
    stop(?REVOLVER, S3),
    stop(?POOLBOY, S4),
    stop(?POOLER, S5),
    ok.

start(?POOLBOY) ->
    Args = [
        {name, {local, ?POOLBOY}},
        {worker_module, ?WORKER_MODULE},
        {size, ?POOL_SIZE},
        {max_overflow, 0}
    ],
    poolboy:start(Args);
start(?POOLER) ->
    pooler:start(),
    PoolConfig = [
        {name, ?POOLER},
        {max_count, ?POOL_SIZE},
        {init_count, ?POOL_SIZE},
        {queue_max, 5000},
        {start_mfa, {?WORKER_MODULE, ?WORKER_FUN, [?WORKER_ARGS]}}
    ],
    pooler:new_pool(PoolConfig);
start(?CUESPORT) ->
    PoolSize = ?POOL_SIZE,
    ChildMF = {?WORKER_MODULE, ?WORKER_FUN},
    cuesport:start_link(?CUESPORT, PoolSize, [], ChildMF, {for_all, [?WORKER_ARGS]});
start(?REVOLVER) ->
    Args = [
        {mfa, {?WORKER_MODULE, ?WORKER_FUN, [?WORKER_ARGS]}},
        {size, ?POOL_SIZE}
    ],
    revolver_pool:start_link(?REVOLVER, Args);
start(?ERLPOOL) ->
    application:ensure_all_started(erlpool),
    Args = [
        {size, ?POOL_SIZE},
        {start_mfa, {?WORKER_MODULE, ?WORKER_FUN, [?WORKER_ARGS]}}
    ],
    erlpool:start_pool(?ERLPOOL, Args).

stop(?ERLPOOL, _S) ->
    erlpool:stop_pool(?ERLPOOL);
stop(?POOLBOY, _S) ->
    poolboy:stop(?POOLBOY);
stop(?POOLER, _S) ->
    pooler:stop();
stop(?REVOLVER, _S) ->
    revolver_pool:stop(?REVOLVER);
stop(?CUESPORT, {ok, Pid}) ->
    exit(Pid, shutdown).

run(?ERLPOOL, Fun) ->
    Fun(erlpool:pid(?ERLPOOL));
run(?CUESPORT, Fun) ->
    Fun(cuesport:get_worker(?CUESPORT));
run(?POOLBOY, Fun) ->
    Worker = poolboy:checkout(?POOLBOY),
    try
        Fun(Worker)
    after
        poolboy:checkin(?POOLBOY, Worker)
    end;
run(?POOLER, Fun) ->
    Worker = pooler:take_member(?POOLER, 50000),
    try
        Fun(Worker)
    after
        pooler:return_member(?POOLER, Worker)
    end;
run(?REVOLVER, Fun) ->
    Fun(revolver_pool:pid(?REVOLVER)).

bench(Module, Number, Concurrency) ->
    Self = self(),
    List = lists:seq(1, Concurrency),
    LoopNumbers = Number div Concurrency,
    Fun = fun(Pid) -> gen_server:call(Pid, get) end,
    %Fun = fun(_Pid) -> ok end,

    A = os:timestamp(),
    Pids = [spawn_link(fun() -> loop(LoopNumbers, Module, Fun), Self ! {self(), done} end) || _ <- List],
    [receive {Pid, done} -> ok end || Pid <- Pids],
    B = os:timestamp(),

    print(Module, Number, A, B).

print(Module, Num, A, B) ->
    Microsecs = timer:now_diff(B, A),
    Time = Microsecs div Num,
    PerSec = case Time of
                 0 ->
                     "N/A";
                 _ ->
                     1000000 div Time
             end,

    io:format("### ~p ~p ms ~p req/sec ~n", [Module, Microsecs div 1000, PerSec]).

loop(0, _Module, _Fun) ->
    ok;
loop(Nr, Module, Fun) ->
    run(Module, Fun),
    loop(Nr-1, Module, Fun).


