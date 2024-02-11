-module(integrity_test_SUITE).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() -> [
    {group, erlpool_group}
].

groups() -> [
    {erlpool_group, [sequence], [
        test_map,
        test_pid_round_robin,
        test_not_existing_pool,
        test_group,
        test_args_include_worker_id,
        test_add_worker
    ]}
].

init_per_suite(Config) ->
    ok = erlpool:start(),
    Args = [{start_mfa, {dummy_worker, start_link, [[]]}}],
    ok = erlpool:start_pool(pool1, [{size, 10} |Args]),
    ok = erlpool:start_pool(pool2, [{size, 5} |Args]),
    Config.

end_per_suite(_Config) ->
    ok = erlpool:stop_pool(pool1),
    %disable info report
    logger:add_handler_filter(default, ?MODULE, {fun(_,_) -> stop end, nostate}),
    %let pool2 to be destroyed by the app supervisor
    erlpool:stop().

test_map(_Config) ->
    L1 = erlpool:map(pool1, fun(X) -> X end),
    L2 = erlpool:map(pool2, fun(X) -> X end),
    Length1 = length(L1),
    Length2 = length(L2),
    Length1 = erlpool:pool_size(pool1),
    Length2 = erlpool:pool_size(pool2),
    ok.

test_pid_round_robin(_Config) ->
    L1 = erlpool:map(pool1, fun(X) -> X end),
    Size = erlpool:pool_size(pool1),

    Fun = fun(_X, Acc) ->
        Pid = erlpool:pid(pool1),
        lists:delete(Pid, Acc)
    end,
    [] = lists:foldl(Fun, L1, lists:seq(1, Size)),
    ok.

test_not_existing_pool(_Config) ->
    {error, not_found} = erlpool:pid(dummy),
    {error, not_found} = erlpool:map(dummy, fun(X) -> X end),
    {error, not_found} = erlpool:pool_size(dummy),
    ok.

test_group(_Config) ->
    Args = [{start_mfa, {dummy_worker, start_link, [[{dummy1, 1}, {dummy2, 2}]]}}],
    ok = erlpool:start_pool('node_pool1_127.0.0.1_6379', [{size, 2}, {group, g1} |Args]),
    ok = erlpool:start_pool(gpool2, [{size, 3}, {group, g1} |Args]),
    ok = erlpool:start_pool(gpool3, [{size, 4}, {group, g2} |Args]),
    ok = erlpool:start_pool(gpool4, [{size, 5}, {group, g2} |Args]),

    PG1 = whereis(erlpool_pool_sup:name('node_pool1_127.0.0.1_6379')),
    PG2 = whereis(erlpool_pool_sup:name(gpool2)),
    PG3 = whereis(erlpool_pool_sup:name(gpool3)),
    PG4 = whereis(erlpool_pool_sup:name(gpool4)),

    true = erlpool:restart_pool('node_pool1_127.0.0.1_6379'),
    false = erlpool:restart_pool(gpool5),

    timer:sleep(100),

    PG11 = whereis(erlpool_pool_sup:name('node_pool1_127.0.0.1_6379')),

    ?assert(PG11 =/= undefined),
    ?assert(PG11 =/= PG1),

    ok = erlpool:restart_group(g2),

    timer:sleep(100),

    NPG1 = whereis(erlpool_pool_sup:name('node_pool1_127.0.0.1_6379')),
    NPG2 = whereis(erlpool_pool_sup:name(gpool2)),
    NPG3 = whereis(erlpool_pool_sup:name(gpool3)),
    NPG4 = whereis(erlpool_pool_sup:name(gpool4)),

    ?assert(NPG1 =:= PG11),
    ?assert(NPG2 =:= PG2),
    ?assert(NPG3 =/= PG3),
    ?assert(NPG4 =/= PG4),

    ok = erlpool:stop_group(g2),

    timer:sleep(100),

    NPG1 = whereis(erlpool_pool_sup:name('node_pool1_127.0.0.1_6379')),
    NPG2 = whereis(erlpool_pool_sup:name(gpool2)),
    undefined = whereis(erlpool_pool_sup:name(gpool3)),
    undefined = whereis(erlpool_pool_sup:name(gpool4)),
    ok.

test_args_include_worker_id(_Config) ->
    Args = [{start_mfa, {dummy_worker, start_link, []}}, {args_include_worker_id, true}],
    ok = erlpool:start_pool(pool_test_args_include_worker_id, [{size, 5} |Args]),

    L = erlpool:map(pool_test_args_include_worker_id, fun(Pid) ->
        gen_server:call(Pid, args)
    end),

    ?assertEqual([1,2,3,4,5], lists:sort(L)),

    ok = erlpool:add_worker(pool_test_args_include_worker_id),

    L2 = erlpool:map(pool_test_args_include_worker_id, fun(Pid) ->
        gen_server:call(Pid, args)
    end),

    ?assertEqual([1,2,3,4,5, 6], lists:sort(L2)),

    ok.

test_add_worker(_Config) ->
    Size = erlpool:pool_size(pool1),
    ok = erlpool:add_worker(pool1),
    NewSize = Size + 1,
    NewSize = erlpool:pool_size(pool1),
    ok.
