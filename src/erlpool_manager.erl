-module(erlpool_manager).
-author("silviu.caragea").

-export([
    init/0,
    register/2
]).

-define(POOL_MANAGER_TAB, erlpool_manager).
-define(GLOBALS_MODULE, erlpool_globals).

init() ->
    ?POOL_MANAGER_TAB == ets:new(?POOL_MANAGER_TAB, [named_table, public, {read_concurrency, true}]).

register(PoolName, PoolSize) ->
    ets:insert(?POOL_MANAGER_TAB, {PoolName, PoolSize}),
    compile_settings(ets:tab2list(?POOL_MANAGER_TAB)).

compile_settings(SettingsList) ->
    code:purge(?GLOBALS_MODULE),
    {module, ?GLOBALS_MODULE} = dynamic_compile:load_from_string(get_settings_code(SettingsList)),
    true.

get_settings_code(SettingsList) ->
    binary_to_list(get_settings_code(SettingsList, <<>>, <<>>)).

get_settings_code([{PoolName, PoolSize}|T], AccHeaders, AccBody) ->
    PoolNameBin = atom_to_binary(PoolName, latin1),
    PoolSizeBin = integer_to_binary(PoolSize),

    NewAccHeaders = <<AccHeaders/binary, PoolNameBin/binary,"/0,">>,
    NewAccBody = <<AccBody/binary, PoolNameBin/binary,"()->", PoolSizeBin/binary, ".\n ">>,
    get_settings_code(T, NewAccHeaders, NewAccBody);
get_settings_code([], AccHeaders0, AccBody) ->
    ModuleBin = atom_to_binary(?GLOBALS_MODULE, latin1),
    ModuleHeader = <<"-module(", ModuleBin/binary, ").\n ">>,
    case AccHeaders0 of
        <<>> ->
            ModuleHeader;
        _ ->
            %remove last comma
            HeaderLength = byte_size(AccHeaders0) - 1,
            <<AccHeaders:HeaderLength/binary, _Rest>> = AccHeaders0,
            <<ModuleHeader/binary, "-export([", AccHeaders/binary, "]).\n ", AccBody/binary>>
    end.