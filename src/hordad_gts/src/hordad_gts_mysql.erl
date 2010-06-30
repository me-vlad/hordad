%%% -------------------------------------------------------------------
%%% File    : hordad_gts_mysql
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: GTS mysql proxy
%%%
%%% Created : 2010-03-16 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_gts_mysql).

-behaviour(gen_server).

%% API
-export([start_link/0,
         transaction/1,
         run_queries/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DRIVER_ID, gts_driver_mysql).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Perform a transaction query
-spec(transaction(function()) ->
             ok | {error, Reason :: any(), Result :: any()}).

transaction(F) when is_function(F) ->
    gen_server:call(?SERVER, {transaction, F}).

%% @doc Perform list of queries in a single transaction
run_queries(Queries) when is_list(Queries) ->
    gen_server:call(?SERVER, {run_queries, Queries}).

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
    DBConfig = hordad_lcf:get_var({hordad_gts, driver_config}),

    case mysql:start_link(?DRIVER_ID,
                          hordad_lib:getv(host, DBConfig),
                          hordad_lib:getv(port, DBConfig),
                          hordad_lib:getv(user, DBConfig),
                          hordad_lib:getv(password, DBConfig),
                          hordad_lib:getv(db, DBConfig)) of
        {error, R} ->
            hordad_log:error(?MODULE, "Error starting db driver (~p):"
                             "~9999p",
                             [hordad_lcf:get_var({hordad_gts, driver}), R]),

            {stop, R};
        {ok, _Pid} ->
            {ok, ?DRIVER_ID}
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
handle_call({transaction, F}, _From, DriverId) ->
    {reply, run_transaction(F, DriverId), DriverId};
handle_call({run_queries, Queries}, _From, DriverId) ->
    Result = run_transaction(fun() ->
                                     lists:foreach(fun(Q) ->
                                                           mysql:fetch(Q)
                                                   end, Queries)
                             end, DriverId),
    {reply, Result, DriverId};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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

%% @doc Workhouse for transaction/1
run_transaction(F, DriverId) when is_function(F) ->
    case mysql:transaction(DriverId, F) of
        {atomic, _Result} ->
            ok;
        {aborted, {Reason, {rollback_result, Result}}} ->
            {error, Reason, Result}
    end.
