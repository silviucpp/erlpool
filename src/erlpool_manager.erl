-module(erlpool_manager).
-author("silviu.caragea").

-define(POOL_MANAGER_TAB, erlpool_manager).
-define(GLOBALS_MODULE, erlpool_globals).

-behaviour(gen_server).

-export([
    start_link/0,
    new_pool/2,
    rem_pool/1,
    %internals
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

new_pool(PoolName, PoolArgs) ->
    gen_server:call(?MODULE, {create_pool, PoolName, PoolArgs}).

rem_pool(PoolName) ->
    gen_server:call(?MODULE, {remove_pool, PoolName}).

init([]) ->
    ?POOL_MANAGER_TAB == ets:new(?POOL_MANAGER_TAB, [named_table, private, {read_concurrency, true}]),
    {ok, #state{}}.

handle_call({create_pool, PoolName, PoolArgs}, _From, State) ->
    {reply, catch create_pool(PoolName, PoolArgs), State};

handle_call({remove_pool, PoolName}, _From, State) ->
    {reply, catch delete_pool(PoolName), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%internals

create_pool(PoolName, PoolArgs) ->
    case proplists:get_value(size, PoolArgs) of
        undefined ->
            {error, <<"pool size not specified">>};
        PoolSize ->
            case erlpool_pool_sup:start_link(PoolName, PoolArgs) of
                {ok, _Pid} ->
                    true = ets:insert(?POOL_MANAGER_TAB, {PoolName, PoolSize}),
                    compile_settings(ets:tab2list(?POOL_MANAGER_TAB));
                Error ->
                    Error
            end
    end.

delete_pool(PoolName) ->
    erlpool_pool_sup:stop(PoolName),
    ets:delete(?POOL_MANAGER_TAB, PoolName),
    compile_settings(ets:tab2list(?POOL_MANAGER_TAB)).

compile_settings(SettingsList) ->
    code:purge(?GLOBALS_MODULE),
    {module, ?GLOBALS_MODULE} = dynamic_compile:load_from_string(get_settings_code(SettingsList)),
    ok.

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