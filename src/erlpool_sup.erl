-module(erlpool_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    add_pool/2,
    remove_pool/1,
    init/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_pool(PoolName, PoolArgs) ->
    ChildSpecs = supervisor_childspec(PoolName, erlpool_pool_sup, infinity, [PoolName,  PoolArgs]),
    supervisor:start_child(?MODULE, ChildSpecs).

remove_pool(PoolName) ->
    case supervisor:terminate_child(?MODULE, PoolName) of
        ok ->
            supervisor:delete_child(?MODULE, PoolName);
        Error ->
            Error
    end.

init([]) ->
    {ok, { {one_for_one, 1000, 1}, []} }.

supervisor_childspec(Name, Module, WaitForClose, Args) ->
    {Name, {Module, start_link, Args}, transient, WaitForClose, supervisor, [Module]}.