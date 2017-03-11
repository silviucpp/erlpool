-module(erlpool_sup).

-behaviour(supervisor).

-export([start_link/0,init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Childrens = [
        proccess(erlpool_manager, infinity)
    ],

    {ok, { {one_for_one, 10, 1}, Childrens} }.

proccess(Name, WaitForClose) ->
    {Name, {Name, start_link, []}, permanent, WaitForClose, worker, [Name]}.