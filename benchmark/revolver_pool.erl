-module(revolver_pool).
-author("silviu.caragea").

-behaviour(supervisor).

-export([start_link/2, init/1, pid/1]).

start_link(Name, Args) ->
    case whereis(revolver_pool_sup) of
        undefined ->
            {ok, _} = supervisor:start_link({local, revolver_pool_sup}, ?MODULE, Args),
            {ok, _} = revolver:start_link(revolver_pool_sup, Name, revolver_options()),
            ok;
        _ ->
            ok
    end.

init(Args) ->
    {mfa, MFA} = proplists:lookup(mfa, Args),
    {size, Size} = proplists:lookup(size, Args),

    Fun = fun(X, Acc) ->
        Name = <<"revolver_pool_worker_", (integer_to_binary(X))/binary>>,
        [children_specs(Name, MFA) | Acc]
    end,

    ChildSpecs = lists:foldl(Fun, [], lists:seq(1, Size)),
    {ok, {{one_for_one, 1000, 1}, ChildSpecs}}.

children_specs(Name, {Module, Fun, Args}) ->
    {Name, {Module, Fun, Args}, transient, 2000, worker, [Module]}.

revolver_options() -> #{
    min_alive_ratio          => 1,
    reconnect_delay          => 10000,
    max_message_queue_length => undefined,
    connect_at_start         => true
}.

pid(Name) ->
    revolver:pid(Name).