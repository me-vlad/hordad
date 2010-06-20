%%% -------------------------------------------------------------------
%%% File    : hordad_dht_storage
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Hordad DHT key-value storage
%%%
%%% Created : 2010-06-21 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------
-module(hordad_dht_storage).

-behaviour(gen_server).

%% API
-export([start_link/0, lookup/1, lookup/2, insert/1, insert/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(STORAGE_DB, hordad_dht_storage).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Lookup key value
-spec(lookup(any()) -> any()).

lookup(Key) ->
    lookup(Key, default).

%% @doc Lookup key value providing default one
-spec(lookup(any(), any()) -> any()).

lookup(Key, Default) ->
    gen_server:call(?SERVER, {lookup, Key, Default}).

%% @doc Insert key-value pair
-spec(insert(any(), any()) -> ok).

insert(Key, Value) ->
    insert([{Key, Value}]).

%% @doc Insert list of key-value pairs
-spec(insert([{any(), any()}]) -> ok).

insert(Data) ->
    gen_server:call(?SERVER, {insert, Data}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    DBPath = hordad_lib:get_file(db,
                                 hordad_lcf:get_var({hordad_dht, storage_db})),

    case hordad_lib_storage:new(?STORAGE_DB, DBPath, 1) of
        {ok, _DB}=Ok ->
            Ok;
        {error, Reason} ->
            hordad_lob:error("Unable to open storage database: ~9999p",
                             [Reason]),
            {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({lookup, Key, Default}, _From, State) ->
    Reply = case dets:lookup(Key) of
                [] ->
                    Default;
                List when is_list(List) ->
                    List;
                {error, Reason} ->
                    hordad_log:warning("Error in DB lookup: ~9999p", [Reason]),
                    Default
            end,

    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
