-module(integrity_test_SUITE).
-author("silviu.caragea").

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() -> [
    {group, erlpool_group}
].

groups() -> [
    {erlpool_group, [sequence], [
        test_map,
        test_pid_round_robin
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
    %let pool2 to be destroyed by the app supervisor
    erlpool:stop().

test_map(_Config) ->
    L1 = erlpool:map(pool1, fun(X) -> X end),
    L2 = erlpool:map(pool2, fun(X) -> X end),
    Length1 = length(L1),
    Length2 = length(L2),
    Length1 = erlpool:pool_size(pool1),
    Length2 = erlpool:pool_size(pool2),
    true.

test_pid_round_robin(_Config) ->
    L1 = erlpool:map(pool1, fun(X) -> X end),
    Size = erlpool:pool_size(pool1),

    Fun = fun(_X, Acc) ->
        Pid = erlpool:pid(pool1),
        lists:delete(Pid, Acc)
    end,
    [] = lists:foldl(Fun, L1, lists:seq(1, Size)),
    true.



