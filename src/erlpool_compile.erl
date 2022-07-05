-module(erlpool_compile).

-define(GLOBALS_MODULE, erlpool_globals).

-export([compile_settings/1]).

-include_lib("syntax_tools/include/merl.hrl").

compile_settings(SettingsList) ->
    Forms = get_settings_code(SettingsList),
    file:write_file("/tmp/erlpool_globals_gen.erl", erl_prettypr:format(erl_syntax:form_list(Forms))),
    case merl:compile_and_load(Forms) of
        {ok, _Bin} ->
            ok;
        Err ->
            Err
    end.

get_settings_code(SettingsList) ->
    ModuleName = ?GLOBALS_MODULE,
    Module = ?Q("-module('@ModuleName@')."),
    Export = ?Q("-export([size/1])."),
    Function = gen(SettingsList, []),
    [Module, Export,  Function].

gen([{Name, Size, _PoolArgs} | R], Acc) ->
    gen(R, [?Q("(_@Name@) -> {ok, _@Size@}") | Acc]);
gen([], Acc) ->
    Clauses = lists:reverse([?Q("(_) -> {error, not_found}") | Acc]),
    erl_syntax:function(merl:term(size), Clauses).

