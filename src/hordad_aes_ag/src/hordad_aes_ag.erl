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
         add_nodes/1,
         add_nodes/2,
         remove_nodes/1,
         remove_nodes/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([get_ldb_tables/0]).

-include_lib("hordad_lib/include/lib_types.hrl").

-define(SERVER, ?MODULE).
-define(TABLE, aes_ag).
-define(ROOM, ?MODULE).
-define(DEFAULT_OWNER, default).

-type(status() :: up | down).
-type(owner() :: atom()).

-record(aes_ag, {
          node,       % Node
          status,     % Node calculated status
          owners      % List of node owners
         }).

-record(state, {
          worker,
          table
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

%% @doc Add nodes to monitoring
-spec(add_nodes([net_node()]) -> ok | {error, any()}).

add_nodes(Nodes) ->
    add_nodes(Nodes, ?DEFAULT_OWNER).

%% @doc Add nodes to monitoring with provided owner
-spec(add_nodes([net_node()], owner()) -> ok | {error, any()}).

add_nodes(Nodes, Owner) ->
    gen_server:call(?SERVER, {add_nodes, Nodes, Owner}).

%% @doc Remove nodes from monitoring
-spec(remove_nodes([net_node()]) -> ok | {error, any()}).

remove_nodes(Nodes) ->
    remove_nodes(Nodes, ?DEFAULT_OWNER).

%% @doc Remove nodes from monitoring with provided owner
-spec(remove_nodes([net_node()], owner()) -> ok | {error, any()}).

remove_nodes(Nodes, Owner) ->
    gen_server:call(?SERVER, {remove_nodes, Nodes, Owner}).

%% @doc Get all nodes status
-spec(status() -> [{net_node(), atom()}]).

status() ->
    gen_server:call(?SERVER, status).

%% @doc Get status of requested node
-spec(status(net_node()) -> status() | undefined).

status(Node) ->
    gen_server:call(?SERVER, {status, Node}).

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
    do_add_nodes(?TABLE, hordad_lcf:get_var({?MODULE, nodes}, []),
                 ?DEFAULT_OWNER),
    ok = hordad_rooms:create(?ROOM),


    {ok, #state{table=?TABLE,
                worker=init_worker(?TABLE)}}.

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
    {reply, get_status(State#state.table), State};
handle_call({status, Node}, _From, State) ->
    {reply, get_status(State#state.table, Node), State};
handle_call({add_nodes, Nodes, Owner}, _From, State) ->
    {reply, do_add_nodes(State#state.table,
                         lists:usort(Nodes), Owner), State};
handle_call({remove_nodes, Nodes, Owner}, _From, State) ->
    {reply, do_remove_nodes(State#state.table,
                            lists:usort(Nodes), Owner), State}.

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
handle_info({'DOWN', Ref, process, Pid, Info},
            #state{worker={Pid, Ref}}=State) ->
    hordad_log:warning(?MODULE, "Worker process died: ~p."
                       "Restarting", [Info]),

    {noreply, State#state{worker=init_worker(State#state.table)}};
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

%% @doc Add nodes to monitor.
%%      If some nodes already in table - update owners if needed
-spec(do_add_nodes(atom(), [net_node()], owner()) -> ok | {error, any()}).

do_add_nodes(Tab, Nodes, Owner) ->
    F = fun() ->
                lists:foreach(
                  fun(Node) ->
                          New =
                              case hordad_ldb:read_nt(Tab, Node) of
                                  [] ->
                                      #aes_ag{node=Node,
                                              status=down,
                                              owners=[Owner]};
                                  [#aes_ag{owners=Owners}=C] ->
                                      C#aes_ag{owners=lists:usort(
                                                        [Owner | Owners])}
                              end,

                          hordad_ldb:write_nt(New)
                  end, Nodes)
        end,

    hordad_ldb:transaction(F).

%% @doc Workhouse for remove_nodes/1
do_remove_nodes(Tab, Nodes, Owner) ->
    F = fun() ->
                lists:foreach(
                  fun(Node) ->
                          case hordad_ldb:read_nt(Tab, Node) of
                              [] ->
                                  ok;
                              [#aes_ag{owners=Owners}=C] ->
                                  case Owners -- [Owner] of
                                      %% No more owners, delete entry
                                      [] ->
                                          hordad_ldb:delete_nt(Tab, Node);
                                      %% Else remove just owner
                                      NewOwners ->
                                          hordad_ldb:write_nt(
                                            C#aes_ag{owners=NewOwners})
                                  end
                          end
                  end, Nodes)
        end,

    hordad_ldb:transaction(F).

%% @doc Worker loop.
%%      Periodically poll all defined pollers
%% @end

worker(Table) ->
    [Period, Pollers, Nodes] = hordad_lcf:get_vars([{?MODULE, cycle_period},
                                                    {?MODULE, pollers},
                                                    {?MODULE, nodes}]),

    timer:sleep(Period),

    aggregate(Nodes, Pollers, Table),

    worker(Table).

%% @doc Poll every poller node provided and make a decision about
%%      node status
%% @end
-spec(aggregate([net_node()], [net_node()], atom()) -> ok).

aggregate(Nodes, Pollers, Table) ->
    Parent = self(),
    Data = [spawn(fun() -> poll(P, Nodes, Parent) end) || P <- Pollers],

    case wait_for_reports(Data, []) of
        timeout ->
            hordad_log:warning(?MODULE, "Poller session timeout", []),
            ok;
        RawData ->
            process_data(analyze(RawData), Table),
            ok
    end.

%% @doc Request poller to init polling procedure and send parent the result
%%      Parent receives message: {net_node(), pid(), error | any()}
-spec(poll(net_node(), [net_node()], pid()) -> ok).

poll({IP, Port}=Poller, Nodes, Parent) ->
    hordad_log:info(?MODULE, "Initiating polling session with ~s",
                    [hordad_lib_fmt:fmt_node(Poller)]),

    try
        {ok, Report} =
          hordad_lib_net:gen_session(IP, Port, "aes_poller", {"poll", Nodes}),

        Parent ! {Poller, self(), Report}
    catch
        _:E ->
            hordad_log:warning(?MODULE, "Error polling ~s: ~p (~p)~n",
                               [hordad_lib_fmt:fmt_node(Poller), E,
                                erlang:get_stacktrace()]),

            Parent ! {Poller, self(), error}
    end,

    ok.

%% @doc Wait for every poller session completes
-spec(wait_for_reports([pid()], list()) -> list() | timeout).

wait_for_reports([], Acc) ->
    Acc;
wait_for_reports(Data, Acc) ->
    SessionTimeout = hordad_lcf:get_var({?MODULE, session_timeout}),

    receive
        {Poller, Pid, Report} ->
            %% Ensure we've got pid in pending list
            case lists:member(Pid, Data) of
                true ->
                    wait_for_reports(Data -- [Pid],
                                     hordad_lib:setv(Poller, Report, Acc));
                _ ->
                    hordad_log:warning(?MODULE,
                                       "Got unexpected poller report from ~s:"
                                       "~p", [hordad_lib_fmt:fmt_node(Poller),
                                              Report]),

                    wait_for_reports(Data, Acc)
            end
    after
        SessionTimeout ->
            timeout
    end.

%% @doc Analyze collected pollers data and return aggregated result
-spec(analyze([{net_node(), status()}]) -> list()).

analyze(Data) ->
    FReport = fun({Node, Status}, Acc) ->
                      Current = hordad_lib:getv(Node, Acc, down),

                      New = if
                                Status == up orelse Current == up ->
                                    up;
                                true ->
                                    Status
                            end,

                      hordad_lib:setv(Node, New, Acc)
              end,

    FPoller = fun({_, error}, Acc) ->
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
    F =
        fun({Node, NewStatus}) ->
                case hordad_ldb:read(Table, Node) of
                    {ok, [#aes_ag{status=OldStatus}=OldEntry]} ->
                        if
                            %% Changed
                            NewStatus =/= OldStatus ->
                                status_handler(Node, OldStatus, NewStatus),

                                hordad_ldb:write(
                                  OldEntry#aes_ag{status=NewStatus});
                            %% Status not changed, just update ts
                            true ->
                                ok
                        end;
                    %% New entry
                    {ok, []} ->
                        hordad_ldb:write(#aes_ag{node=Node, status=NewStatus,
                                                 owners=[?DEFAULT_OWNER]})
                end
        end,

    lists:foreach(F, Data).

%% @doc Handle changes in node status

-spec(status_handler(net_node(), Old :: status(), New :: status()) -> ok).

status_handler(Node, Old, New) ->
    hordad_rooms:send(?ROOM, {?MODULE, status, Node, Old, New, now()}),

    hordad_log:info(?MODULE,
                    "Node ~s status changed ~p -> ~p",
                    [hordad_lib_fmt:fmt_node(Node), Old, New]).

%% @doc Get all nodes status. Workhouse for status/0
get_status(Table) ->
    {ok, Data} = hordad_ldb:foldl(
                   fun(#aes_ag{node=Node, status=Status}, Acc) ->
                           [{Node, Status} | Acc]
                   end, [], Table),

    Data.

%% @doc Get status of requested node. Workhouse for status/1
get_status(Table, Node) ->
    case hordad_ldb:read(Table, Node) of
        {ok, []} ->
            undefined;
        {ok, [#aes_ag{status=Status}]} ->
            Status
    end.

init_worker(Table) ->
    spawn_monitor(fun() -> worker(Table) end).
