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

-export([get_ldb_tables/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TABLE, registrar).

-record(registrar, {tag, % Handler tag
                    cb   % Callback - {Module, Function, Args}
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

%% @doc hordad_ldb table info callback.
-spec(get_ldb_tables() -> {Name :: atom(), Attrs :: [{atom(), any()}]}).

get_ldb_tables() ->
    {?TABLE, [{attributes, record_info(fields, registrar)}]}.

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, ?TABLE}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({register, Tag, CB}, _From, Table) ->
    reg(Tag, CB),

    {reply, ok, Table};
handle_call({unregister, Tag}, _From, Table) ->
    unreg(Tag, Table),

    {reply, ok, Table};
handle_call(registered, _From, Table) ->
    {reply, hordad_ldb:match(#registrar{tag='_', cb='_'}), Table};
handle_call({get_cb, Tag}, _From, Table) ->
    {ok, Reply} = hordad_ldb:read(Table, Tag),
    
    case Reply of
        [] ->
            undefined;
        [#registrar{cb=CB}] ->
            CB
    end,

    {reply, Reply, Table}.

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

%% @doc Register new handler
-spec(reg(tag(), handler_cb()) -> ok).

reg(Tag, CB) ->
    ok = hordad_ldb:write(#registrar{tag=Tag, cb=CB}),
    ok.

%% @doc Unregister handler
unreg(Tag, Table) ->
    ok = hordad_ldb:delete(Table, Tag),
    ok.
