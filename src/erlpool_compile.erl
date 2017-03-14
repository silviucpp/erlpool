
-module(erlpool_compile).
-author("silviu.caragea").

-define(GLOBALS_MODULE, erlpool_globals).

-export([compile_settings/1]).

compile_settings(SettingsList) ->
    code:purge(?GLOBALS_MODULE),
    case dynamic_compile:load_from_string(get_settings_code(SettingsList)) of
        {module, ?GLOBALS_MODULE} ->
            ok;
        Error ->
            Error
    end.

get_settings_code(SettingsList) ->
    binary_to_list(get_settings_code(SettingsList, <<>>, <<>>)).

get_settings_code([{PoolName, PoolSize, _PoolGroup}|T], AccHeaders, AccBody) ->
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