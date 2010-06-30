%%% -------------------------------------------------------------------
%%% File    : hordad_registrar.erl
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Application supervisor
%%%
%%% Created : 2010-01-04 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_registrar).

-behaviour(gen_server).

%% API
-export([start_link/0,
         register/2,
         unregister/1,
         registered/0,
         get_cb/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(DEFAULT_DB_NAME, "hordad_registrar.db").

-record(handler, {tag, % Handler tag
                  cb   % Callback - {Module, Function, Args}
                 }).

-record(state, {table, % Table id
                path   % Path to db file
               }).
          
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-type(tag() :: list()).
-type(handler_cb() :: {Module :: atom(), Fun :: atom(), Args :: [any()]}).

%% @doc Register new tag handler
-spec(register(tag(), handler_cb()) -> ok | {error, string()}).

register(Tag, CB) ->
    gen_server:call(?MODULE, {register, Tag, CB}).

%% @doc Unregister tag handler
-spec(unregister(tag()) -> ok).
             
unregister(Tag) ->
    gen_server:call(?MODULE, {unregister, Tag}).

%% @doc Get list of registered handlers
registered() ->
    gen_server:call(?MODULE, registered).

%% @doc Get registered callback, if any
-spec(get_cb(tag()) -> handler_cb() | undefined).

get_cb(Tag) ->
    gen_server:call(?MODULE, {get_cb, Tag}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    DBPath = hordad_lib:get_file(db,
                             hordad_lcf:get_var({hordad_registrar, db},
                                                ?DEFAULT_DB_NAME)),

    Tab = case ets:file2tab(DBPath) of
              {ok, T} ->
                  T;
              {error, Reason} ->
                  hordad_log:log(?MODULE, info,
                                 "Unable to open registrar file ~s: ~9999p. "
                                 "Creating new one.", [DBPath, Reason]),
                  ets:new(list_to_atom(?DEFAULT_DB_NAME),
                          [ordered_set, private, {keypos, #handler.tag}])
          end,

    State = #state{table=Tab, path=DBPath},

    % Register entries
    lists:foreach(fun(Entry) ->
                          reg(Entry#handler.tag, Entry#handler.cb, State)
                  end, ets:tab2list(Tab)),

    {ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({register, Tag, CB}, _From, State) ->
    reg(Tag, CB, State),

    {reply, ok, State};
handle_call({unregister, Tag}, _From, State) ->
    unreg(Tag, State),

    {reply, ok, State};
handle_call(registered, _From, #state{table=Tab}=State) ->
    {reply, ets:tab2list(Tab), State};
handle_call({get_cb, Tag}, _From, #state{table=Tab}=State) ->
    Reply = case ets:lookup(Tab, Tag) of
                [] ->
                    undefined;
                [#handler{cb=CB}] ->
                    CB
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
terminate(Reason, State) ->
    hordad_log:info(?MODULE, "Shutting down: ~p~n", Reason),
    sync_tab(State),
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

%% @doc Register new handler
-spec(reg(tag(), handler_cb(), #state{}) -> ok).

reg(Tag, CB, #state{table=Tab}=State) ->
    ets:insert(Tab, #handler{tag=Tag, cb=CB}),
    sync_tab(State),
    ok.

%% @doc Unregister handler
unreg(Tag, #state{table=Tab}=State) ->
    ets:delete(Tab, Tag),
    sync_tab(State),
    ok.

%% @doc Dump ets contents to file
-spec(sync_tab(#state{}) -> ok | {error, string()}).
              
sync_tab(#state{table=Tab, path=File}) ->
    ets:tab2file(Tab, File).
