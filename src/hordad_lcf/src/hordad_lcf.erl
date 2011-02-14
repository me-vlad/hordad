%%% -------------------------------------------------------------------
%%% File    : hordad_lcf.erl
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Local configuration facility server
%%%
%%% Created : 2010-01-04 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_lcf).

-behaviour(gen_server).

%% API
-export([start_link/0,
         get_var/1,
         get_var/2,
         set_var/2,
         get_vars/1,
         get_vars/2,
         get_all/0,
         reload/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(ROOM, ?MODULE).

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

%% @doc Get local configuration parameter value
-spec(get_var(any()) -> Val :: any() | undefined).

get_var(Var) ->
    gen_server:call(?MODULE, {get_var, Var, undefined}).

%% @doc Get local configuration parameter value or default if undefined
-spec(get_var(any(), any()) -> Val :: any() | any()).

get_var(Var, Default) ->
    gen_server:call(?MODULE, {get_var, Var, Default}).

%% @doc Get a list of parameters at once
-spec(get_vars([any()]) -> [any()]).
             
get_vars(Vars) ->
    [get_var(Var) || Var <- Vars].

%% @doc Get a list of parameters at once. Substitude with default if undefined
-spec(get_vars([tuple()], any()) -> [any()]).

get_vars(Vars, Default) ->
    [get_var(Var, Default) || Var <- Vars].

%% @doc Get proplist of all local variables
-spec(get_all() -> [any()]).

get_all() ->
    gen_server:call(?MODULE, get_all).

%% @doc Reload configuration
-spec(reload() -> ok | {error, string()}).

reload() ->
    gen_server:call(?MODULE, reload).

%% @doc Set new local variable. Variable does not get written into
%%      lcf.conf file, so it will exist only until hordad_lcf restarts
%% @end.
-spec(set_var(any(), any()) -> ok).

set_var(Var, Val) ->
    gen_server:call(?MODULE, {set_var, Var, Val}).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    case load_config() of
        {ok, Data} ->
            {ok, Data};
        {error, Reason} ->
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
handle_call({get_var, Var, Default}, _From, Data) ->
    Reply = case dict:find(Var, Data) of
                error ->
                    Default;
                {ok, Val} ->
                    Val
            end,

    {reply, Reply, Data};
handle_call(get_all, _From, Data) ->
    {reply, dict:to_list(Data), Data};
handle_call({set_var, Var, Val}, _From, Data) ->
    {reply, ok, do_set_var(Var, Val, Data)};
handle_call(reload, _From, OldData) ->
    Reply = case load_config() of
                {ok, NewData} ->
                    {reply, ok, NewData};
                {error, Reason} ->
                    {reply, {error, Reason}, OldData}
            end,

    Reply.

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

%% @doc Load confguration file
-spec(load_config() -> {ok, dict()} | {error, string()}).
             
load_config() ->
    {ok, Conf} = hordad_lib:get_conf_file(lcf),

    case file:consult(Conf) of
        {ok, Data} ->
            notify_room(Data),

            {ok, dict:from_list(Data)};
        {error, Reason} ->
            {error, file:format_error(Reason)}
    end.

%% @doc Send {lcf, set, Key, Value} messages to lcf room for every l,v pair
-spec(notify_room([{any(), any()}]) -> ok).

notify_room(Data) ->
    % First check if hordad_rooms is running

    case whereis(hordad_rooms) of
        undefined ->
            ok;
        _ ->
            lists:foreach(fun({K, V}) ->
                                  hordad_rooms:send(?ROOM, {?ROOM, set, K, V})
                          end, Data),
            ok
    end.

%% @doc Workhouse for store/2

do_set_var(Var, Val, Data) ->
    notify_room([{Var, Val}]),
    dict:store(Var, Val, Data).
