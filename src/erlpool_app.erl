-module(erlpool_app).
-author("silviu.caragea").

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    true = erlpool_manager:init(),
    erlpool_sup:start_link().

stop(_State) ->
    ok.


