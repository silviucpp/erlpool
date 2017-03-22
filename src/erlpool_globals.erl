-module(erlpool_globals).
-author("silviu.caragea").

%% Module stub.
%% Will be replaced by the module with the same name at runtime.
%% The only purpose for the module is to suppress warnings from code analyzers,
%% as dynamically compiled module is not available during the build.

-export([]).

-on_load(do_not_load/0).

do_not_load() ->
    do_not_load.