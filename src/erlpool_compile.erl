-module(erlpool_compile).

-define(GLOBALS_MODULE, erlpool_globals).

-export([compile_settings/1]).

compile_settings(SettingsList) ->
    case merl:compile_and_load(get_settings_code(SettingsList)) of
        {ok, _Bin} ->
            ok;
        Err ->
            Err
    end.

get_settings_code(SettingsList) ->
    Module = io_lib:format("-module(~s).", [?GLOBALS_MODULE]),
    Export = "-export([size/1]).",
    Functions =
        lists:foldl(
            fun({Name, Size, _Group}, Acc) ->
                [io_lib:format("size(~s) -> {ok, ~b};", [Name, Size]) | Acc]
            end, ["size(_) -> {error, not_found}."], SettingsList),
    merl:quote([Module, Export | Functions]).

