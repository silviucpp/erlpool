-module(erlpool_utils).

-export([env/1]).

env(Attr) ->
    case application:get_env(erlpool, Attr) of
        {ok, Value} ->
            Value;
        _ ->
            undefined
    end.
