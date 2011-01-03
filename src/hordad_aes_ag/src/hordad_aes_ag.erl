%%% -------------------------------------------------------------------
%%% File    : hordad_aes_ag_sup
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: AES aggregator
%%%
%%% Created : 2010-02-10 by Max E. Kuznecov <mek@mek.uz.ua>
%%% -------------------------------------------------------------------

-module(hordad_aes_ag).

-behaviour(gen_server).

%% API
-export([start_link/0,
         status/0,
         status/1,
         lar/1,
         report/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([get_ldb_tables/0]).

-include_lib("hordad_aes_agent/include/hordad_aes_agent.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_CYCLE_PERIOD, 60000).
-define(TABLE, aes_ag).

-record(aes_ag, {
          node,       % Node IP
          report,     % Node report (#agent_report)
          status      % Node calculated status
         }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Get all nodes status
-spec(status() -> [{tuple(), atom(), #agent_report{}}]).

status() ->
    gen_server:call(?SERVER, status).

%% @doc Get status of requested node
-spec(status(tuple()) -> available | down | undefined).

status(IP) ->
    gen_server:call(?SERVER, {status, IP}).

%% @doc Get LAR value of requested node
-spec(lar(tuple()) -> integer()).

lar(IP) ->
    gen_server:call(?SERVER, {lar, IP}).

%% @doc Get latest poller report for requested node
-spec(report(tuple()) -> #agent_report{}).

report(IP) ->
    gen_server:call(?SERVER, {report, IP}).

%% @doc hordad_ldb table info callback.
-spec(get_ldb_tables() -> {Name :: atom(), Attrs :: [{atom(), any()}]}).

get_ldb_tables() ->
    {?TABLE, [{attributes, record_info(fields, aes_ag)}]}.

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
    spawn(fun() -> worker(?TABLE) end),

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
handle_call(status, _From, State) ->
    {reply, get_status(State), State};
handle_call({status, IP}, _From, State) ->
    {reply, get_status(State, IP), State};
handle_call({lar, IP}, _From, State) ->
    {reply, get_lar(State, IP), State};
handle_call({report, IP}, _From, State) ->
    {reply, get_report(State, IP), State}.

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

%% @doc Worker loop.
%%      Periodically poll all defined pollers
%% @end

worker(Table) ->
    Period = hordad_lcf:get_var({?MODULE, cycle_period},
                                ?DEFAULT_CYCLE_PERIOD),

    receive
    after
        Period ->
            aggregate(hordad_lcf:get_var({?MODULE, pollers}, []), Table),
            worker(Table)
    end.

%% @doc Poll every poller node provided and make a decision about
%%      node status
%% @end
-type(ip() :: {integer(), integer(), integer(), integer()}).
-spec(aggregate([ip()], atom()) -> ok).

aggregate(Pollers, Table) ->
    Dict = lists:foldl(fun(Node, Acc) ->
                               Ref = make_ref(),
                               Parent = self(),

                               dict:store(
                                 Node,
                                 {Ref,
                                  spawn(fun() ->
                                                poll(Node, Ref, Parent)
                                        end)}, Acc)
                       end, dict:new(), Pollers),

    case wait_for_reports(Pollers, Dict) of
        timeout ->
            hordad_log:warning(?MODULE, "Poller session timeout", []),
            ok;
        RawData ->
            AnalyzedData = analyze(dict:to_list(RawData)),
            process_data(AnalyzedData, Table),
            ok
    end.

%% @doc Request poller to init polling procedure and send parent the result
%%      Parent receives message: {reference(), ip(), pid(), error | any()}
-spec(poll(ip(), reference(), pid()) -> ok).

poll(Poller, Ref, Parent) ->
    hordad_log:info(?MODULE, "Initiating polling session with ~p", [Poller]),

    try
        {ok, {ok, Ref, Report}} =
          hordad_lib_net:gen_session(Poller, "aes_poller", {"poll", Ref}),

        Parent ! {Ref, Poller, self(), Report}
    catch
        _:E ->
            hordad_log:warning(?MODULE, "Error polling ~p: ~9999p (~9999p)~n",
                               [Poller, E, erlang:get_stacktrace()]),

            Parent ! {Ref, Poller, self(), error}
    end,
    
    ok.

%% @doc Wait for every poller session completes
-spec(wait_for_reports([ip()], dict()) -> dict() | timeout).

wait_for_reports([], Dict) ->
    Dict;
wait_for_reports(Pollers, Dict) ->
    SessionTimeout = hordad_lcf:get_var({?MODULE, session_timeout}),

    receive
        {Ref, Poller, Pid, Report} ->
            %% Ensure we've got poller in pending list
            case dict:find(Poller, Dict) of
                {ok, {Ref, Pid}} ->
                    wait_for_reports(Pollers -- [Poller],
                                     dict:store(Poller, Report, Dict));
                _ ->
                    hordad_log:warning(?MODULE,
                                       "Got unexpected poller report from ~p:"
                                       "~9999p", [Poller, Report]),
                    wait_for_reports(Pollers, Dict)
            end
    after
        SessionTimeout ->
            timeout
    end.

%% @doc Analyze collected pollers data and return aggregated result
-spec(analyze(list()) -> list()).

analyze(Data) ->
    FReport = fun({Node, Report}, Acc) ->
                      Current = hordad_lib:getv(Node, Acc, #agent_report{}),

                      New = if
                                Report#agent_report.status == available ->
                                    Report;
                                Current#agent_report.status == available ->
                                    Current;
                                true ->
                                    Report
                            end,

                      hordad_lib:setv(Node, New, Acc)
              end,

    FPoller = fun({Poller, error}, Acc) ->
                      hordad_log:warning(?MODULE,
                                         "Skipping erroneus report for "
                                         "poller ~9999p", [Poller]),
                      Acc;
                 ({_Poller, Report}, Acc) ->
                      lists:foldl(FReport, Acc, Report)
              end,

    lists:foldl(FPoller, [], Data).

%% @doc Compare stored data and newly collected one and trigger
%       appropriate handlers
%% @end
-spec(process_data(list(), atom()) -> ok).

process_data(Data, Table) ->
    F = fun({Node, Report}, Acc) ->
                NewStatus = calc_status(Node, Report),

                case hordad_ldb:read(Table, Node) of
                    [#aes_ag{status=OldStatus}=OldEntry] ->
                        {NewAcc, NewEntry} =
                            if
                                % Changed
                                NewStatus =/= OldStatus ->
                                    status_handler(Node, OldStatus, NewStatus),

                                    {[Node | Acc],
                                     OldEntry#aes_ag{status=NewStatus,
                                                     report=Report}};
                                % Status not changed, just update report
                                true ->
                                    {Acc, OldEntry#aes_ag{report=Report}}
                            end,

                        hordad_ldb:write(NewEntry),

                        NewAcc;
                    %% New entry
                    [] ->
                        hordad_ldb:write(#aes_ag{node=Node, status=NewStatus,
                                                 report=Report}),
                        Acc;
                    %% No change
                    _ ->
                        Acc
                end
        end,

    Acc = lists:foldl(F, [], Data),

    %% Check if there were pending nodes
    Affected = case get(pending) of
                   undefined ->
                       Acc;
                   Pending ->
                       lists:usort(Pending ++ Acc)
               end,

    %% Ensure GTS has successfully processed data,
    %% otherwise put it into pending list and try next time
    try
        ok = hordad_gts:process_data(Affected),
        erase(pending)
    catch
        _:E ->
            hordad_log:error(?MODULE, "Error processing data, "
                             "saving for the next interation: ~9999p", [E]),
            put(pending, Affected)
    end.

%% @doc Handle changes in node status

-spec(status_handler(ip(), Old :: atom(), New :: atom()) -> atom()).

status_handler(Node, available, down) ->
    hordad_log:info(?MODULE,
                    "Node ~p status changed available -> down", [Node]);
status_handler(Node, down, available) ->
    hordad_log:info(?MODULE,
                    "Node ~p status changed down -> available", [Node]);
status_handler(Node, Old, New) ->
    hordad_log:warning(?MODULE, "Unexpected node ~p state change: "
                       "~9999p -> ~9999p", [Node, Old, New]).

%% @doc Get all nodes status. Workhouse for status/0
get_status(Table) ->
    hordad_ldb:foldl(
      fun(#aes_ag{node=Node, status=Status, report=Report}, Acc) ->
              [{Node, Status, Report} | Acc]
      end, [], Table).

%% @doc Get status of requested node. Workhouse for status/1
get_status(Table, Node) ->
    case hordad_ldb:read(Table, Node) of
        {ok, []} ->
            undefined;
        {ok, [#aes_ag{status=Status}]} ->
            Status
    end.

%% @doc Get LAR of requested node. Workhouse for lar/1
get_lar(Table, Node) ->
    case get_report(Table, Node) of
        undefined ->
            undefined;
        #agent_report{lar=LAR} ->
            LAR
    end.

%% @doc Get report of requested node. Workhouse for report/1
get_report(Table, Node) ->
    case hordad_ldb:read(Table, Node) of
        {ok, []} ->
            undefined;
        {ok, [#aes_ag{report=Report}]} ->
            Report
    end.

%% @doc Calculate overall node status, including availability status
%% and LAR value
%% @end
-spec(calc_status(ip(), #agent_report{}) -> down | available).

calc_status(Node, NewReport) ->
    NodeMaxLar = hordad_gts_lib:get_max_lar(Node),

    if
        %% Node availability status down
        NewReport#agent_report.status =:= down ->
            down;
        % LAR exceeded threshold
        NewReport#agent_report.lar > NodeMaxLar ->
            down;
        % Else node is available
        true ->
            available
    end.
