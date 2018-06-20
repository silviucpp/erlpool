-module(erlpool_compile).

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
    binary_to_list(get_settings_code(SettingsList, <<>>)).

get_settings_code([{PoolName, PoolSize, _PoolGroup}|T], AccBody) ->
    PoolNameBin = atom_to_binary(PoolName, latin1),
    PoolSizeBin = integer_to_binary(PoolSize),
    ModuleFun = <<"size('", PoolNameBin/binary,"') -> {ok, ", PoolSizeBin/binary,"};\n">>,
    get_settings_code(T, <<AccBody/binary, ModuleFun/binary>>);
get_settings_code([], AccBody0) ->
    AccBody = <<AccBody0/binary, "size(_) -> {error, not_found}.\n">>,
    ModuleBin = atom_to_binary(?GLOBALS_MODULE, latin1),
    ModuleHeader = <<"-module(", ModuleBin/binary, ").\n -export([size/1]).\n">>,
    <<ModuleHeader/binary, AccBody/binary>>.