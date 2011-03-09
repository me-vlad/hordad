%%% -------------------------------------------------------------------
%%% File    : hordad_ddb_lookup
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: DDB lookup layer
%%%
%%% Created : 2010-11-01 by Max E. Kuznecov <mek@mek.uz.ua>
%%% -------------------------------------------------------------------

-module(hordad_ddb_lookup).

-behaviour(gen_server).

%% API
-export([start_link/0,
         set_successor/1,
         set_predecessor/1,
         get_self/0,
         get_successor/0,
         get_predecessor/0,
         get_full_circle/0,
         get_full_circle/1,
         lookup/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([service_handler/2]).

-include("hordad_ddb_lookup.hrl").

-define(SERVER, ?MODULE).
-define(SERVICE_TAG, "ddb_lookup").
-define(ERROR_RETRY_INTERVAL, 5000).

-record(state, {
          self,
          successor,
          predecessor,
          finger_table,
          successor_list,
          join,
          stabilizer,
          finger_checker
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

%% @doc Set current node's successor
-spec(set_successor(#node{} | undefined) -> ok).

set_successor(Node) when is_record(Node, node); Node == undefined ->
    gen_server:call(?SERVER, {set_successor, Node}).

%% @doc Set current node's predecessor
-spec(set_predecessor(#node{} | undefined) -> ok).

set_predecessor(Node) when is_record(Node, node); Node == undefined ->
    gen_server:call(?SERVER, {set_predecessor, Node}).

%% @doc Get current node
-spec(get_self() -> #node{}).

get_self() ->
    gen_server:call(?SERVER, get_self).

%% @doc Get current node's successor
-spec(get_successor() -> #node{}).

get_successor() ->
    gen_server:call(?SERVER, get_successor).

%% @doc Get current node's predecessor
-spec(get_predecessor() -> #node{} | undefined).

get_predecessor() ->
    gen_server:call(?SERVER, get_predecessor).

%% @doc Find successor nodes for provided Ids
-spec(lookup(node_id() | [node_id()]) -> {ok, #node{} | [#node{}]}
                                             | {error, any()}).

lookup(Id) when is_integer(Id) ->
    case lookup([Id]) of
        {error, _}=E ->
            E;
        {ok, [#node{}=Succ]} ->
            {ok, Succ}
    end;
lookup(Ids) when is_list(Ids) ->
    Self = get_self(),

    case collect_successors(Self, Ids) of
        {error, Reason, _, _, _} ->
            {error, Reason};
        Nodes ->
            {ok, [N || {_, N} <- Nodes]}
    end.

%% @doc Get list of all nodes in the circle starting with current one
-spec(get_full_circle() -> [node_info()]).

get_full_circle() ->
    get_full_circle(get_self()).

%% @doc Get list of all nodes in the circle starting with provided one
-spec(get_full_circle(#node{}) -> [node_info()]).

get_full_circle(StartNode) ->
    get_full_circle(StartNode, StartNode, []).

get_full_circle(StartNode, StartNode, [_|_]=Acc) ->
    lists:reverse(Acc);
get_full_circle(Node, StartNode, Acc) ->
    case session(Node, ?SERVICE_TAG, "get_node_info") of
        {ok, {_Pred, Node, Succ, _FT}=Info} ->
            get_full_circle(Succ, StartNode, [Info | Acc]);
        _ ->
            get_full_circle(StartNode, StartNode, Acc)
    end.

%% @doc Service handler callback
service_handler({"find_successor", Ids}, _Socket) ->
    gen_server:call(?SERVER, {find_successor, lists:usort(Ids)});
service_handler("get_successor", _Socket) ->
    get_successor();
service_handler("get_predecessor", _Socket) ->
    get_predecessor();
service_handler({"notify", Node}, _Socket) ->
    gen_server:call(?SERVER, {notify, Node});
service_handler("get_node_info", _Socket) ->
    gen_server:call(?SERVER, get_node_info);
service_handler({"join", Node}, _Socket) ->
    Succ = get_successor(),
    Self = get_self(),

    if
        %% Bootstrap node case
        Node /= Self andalso Succ == Self ->
            set_successor(Node);
        true ->
            ok
    end,

    ok.

%%====================================================================
%% Private functions
%%====================================================================

%% @doc Return finger table in current state
-spec(get_finger_table() -> finger_table()).

get_finger_table() ->
    gen_server:call(?SERVER, get_finger_table).

%% @doc Update finger table with new values
-spec(update_finger_table([{node_id(), #node{}}]) -> ok).

update_finger_table(Values) ->
    gen_server:call(?SERVER, {update_finger_table, Values}).

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
    hordad_registrar:register(?SERVICE_TAG,
                              {hordad_service, generic_service_handler,
                               [?MODULE, service_handler, []]}),

    ok = hordad_rooms:join(hordad_aes_ag, self()),

    [IP, Port] = hordad_lcf:get_vars([{hordad, bind_ip}, {hordad, bind_port}]),
    Self = hordad_ddb_lib:make_node(IP, Port),

    {ok, #state{
       self = Self,
       successor = Self,
       predecessor = undefined,
       successor_list = [],
       join = init_join(Self),
       finger_table = init_finger_table(Self#node.id),
       stabilizer = init_stabilizer(),
       finger_checker = init_finger_checker()
      }
    }.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({set_successor, Node}, _From, State) ->
    {reply, ok, do_set_successor(Node, State)};
handle_call({set_predecessor, Node}, _From, State) ->
    {reply, ok, State#state{predecessor=Node}};
handle_call(get_self, _From, #state{self=Self}=State) ->
    {reply, Self, State};
handle_call(get_successor, _From, #state{successor=Succ}=State) ->
    {reply, Succ, State};
handle_call(get_predecessor, _From, #state{predecessor=Pred}=State) ->
    {reply, Pred, State};
handle_call({notify, Node}, _From, State) ->
    {reply, ok, do_notify(Node, State)};
handle_call(get_node_info, _From,
            #state{self=Self, successor=Succ,
                   predecessor=Pred, finger_table=FT}=State) ->
    {reply, {Pred, Self, Succ, compress_finger_table(FT)}, State};
handle_call({find_successor, Ids}, _From, State) ->
    {reply, do_find_successor(Ids, State), State};
handle_call(get_finger_table, _From, #state{finger_table=FT}=State) ->
    {reply, FT, State};
handle_call({update_finger_table, Values}, _From,
            #state{finger_table=FT}=State) ->
    NewFT = lists:foldl(fun({Id, _Node}=Val, AccFT) ->
                              lists:keyreplace(Id, 1, AccFT, Val)
                      end, FT, Values),

    {reply, ok, State#state{finger_table=NewFT}}.

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
            #state{stabilizer={Pid, Ref}}=State) ->
    hordad_log:warning(?MODULE, "Stabilizer process died: ~p. Restarting",
                       [Info]),

    {noreply, State#state{stabilizer=init_stabilizer()}};

handle_info({'DOWN', Ref, process, Pid, Info},
            #state{finger_checker={Pid, Ref}}=State) ->
    hordad_log:warning(?MODULE, "Finger checker process died: ~p."
                       "Restarting", [Info]),

    {noreply, State#state{finger_checker=init_finger_checker()}};

handle_info({'DOWN', Ref, process, Pid, Info},
            #state{join={Pid, Ref}, self=Self}=State) ->
    NewState = case Info of
                   normal ->
                       State;
                   _ ->
                       hordad_log:warning(?MODULE,
                                          "Join process died: ~p."
                                          "Restarting", [Info]),
                       State#state{join=init_join(Self)}
               end,

    {noreply, NewState};

handle_info({hordad_aes_ag, status, Node, _Old, New, _},
            #state{predecessor=Pred}=State) ->
    NewState =
        if
            %% Our predecessor is down
            Node == Pred andalso New == down ->
                hordad_log:info(?MODULE, "Predecessor is down", []),
                State#state{predecessor=undefined};
            %% Some other node changed state, ignore
            true ->
                State
        end,

    {noreply, NewState};

handle_info(Msg, State) ->
    hordad_log:warning(?MODULE, "Unknown message received: ~p", [Msg]),

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

%% @doc Init join procedure
init_join(Self) ->
    {IP, Port} = hordad_lcf:get_var({hordad_ddb, entry_point}),

    spawn_monitor(fun() ->
                          join(hordad_ddb_lib:make_node(IP, Port), Self)
                  end).

%% @doc Join network
join(#node{ip=Ip, port=Port}=Entry, #node{id=Id}=Self) ->
    hordad_log:info(?MODULE, "Trying to join existing overlay network "
                    "using ~p:~p as entry point", [Ip, Port]),

    case collect_successors(Entry, [Id]) of
        {error, Reason, _, _, _} ->
            hordad_log:error(?MODULE, "Unable to join overlay network: ~p",
                             [Reason]),

            timer:sleep(?ERROR_RETRY_INTERVAL),

            join(Entry, Self);
        [{Id, Succ}] when is_record(Succ, node)  ->
            hordad_log:info(?MODULE, "Found successor: ~p:~p (~p)",
                            [Succ#node.ip, Succ#node.port, Succ#node.id_str]),

            spawn(fun() -> notify_successor(Succ, Self) end),
            set_successor(Succ)
    end.

%% @doc Collect successors for a list of provided ids
-spec(collect_successors(#node{}, [node_id()]) ->
             {error, any(), CurrentNode :: #node{},
              Rest :: [node_id()], Found :: [{node_id(), #node{}}]} |
             [{node_id(), #node{}}]).

collect_successors(Node, Rest) ->
    collect_successors(Node, Rest, []).

collect_successors(_, [], FoundAcc) ->
    FoundAcc;
collect_successors(Node, Rest, FoundAcc) ->
    case session(Node, ?SERVICE_TAG, {"find_successor", Rest}) of
        {error, E} ->
            {error, E, Node, Rest, FoundAcc};
        {ok, {Found, NotFound}} ->
            NewFoundAcc = Found ++ FoundAcc,

            case NotFound of
                [] ->
                    NewFoundAcc;
                [{NextNode, NewRest} | _] ->
                    collect_successors(NextNode, NewRest, NewFoundAcc)
            end
    end.

%% @doc Workhouse for find_successor/1
do_find_successor(Ids, #state{self=Self, successor=Succ}=State) ->
    {Found, RawNotFound} =
        lists:foldr(
          fun(Id, {Found, NotFound}) ->
                  InRange = hordad_ddb_lib:between_right_inc(
                              Self#node.id, Succ#node.id, Id),

                  if
                      InRange == true ->
                          {[{Id, Succ} | Found], NotFound};
                      %% Search in finger table
                      true ->
                          Next = closest_preceding_node(Id, State),

                          if
                              %% No other node found, return self
                              Next == Self ->
                                  {[{Id, Self} | Found], NotFound};
                              true ->
                                  Cur = hordad_lib:getv(Next, NotFound, []),

                                  {Found,
                                   hordad_lib:setv(Next, [Id | Cur], NotFound)}
                          end
                  end
          end, {[], []}, Ids),

    %% Sort NotFound list so that the node with maximum amount of next
    %% references is first
    {Found, lists:sort(fun({_, Ids1}, {_, Ids2}) ->
                               length(Ids1) >= length(Ids2)
                       end, RawNotFound)}.

%% @doc Find the closest preceding node according to finger table info
-spec(closest_preceding_node(node_id(), #state{}) -> #node{}).

closest_preceding_node(Id, #state{self=Self, finger_table=FT}) ->
    find_preceding_node(Id, Self#node.id, lists:reverse(FT), Self).

find_preceding_node(_, _, [], Def) ->
    Def;
find_preceding_node(Id, SelfId, [{_, Node} | T], Def)
  when is_record(Node, node) ->
    case hordad_ddb_lib:between(SelfId, Id, Node#node.id) of
        true ->
            Node;
        _ ->
            find_preceding_node(Id, SelfId, T, Def)
    end;
find_preceding_node(Id, SelfId, [_ | T], Def) ->
    find_preceding_node(Id, SelfId, T, Def).

%% @doc Init stabilizer process
init_stabilizer() ->
    erlang:spawn_monitor(fun stabilizer/0).

%% @doc Init finger checker process
init_finger_checker() ->
    erlang:spawn_monitor(fun finger_checker/0).

%% @doc Init finger table
-spec(init_finger_table(node_id()) -> finger_table()).

init_finger_table(Id) ->
    [{Id + round(math:pow(2, X - 1)) rem ?MODULO, undefined} ||
        X <- lists:seq(1, ?M)].

%% @doc Remove empty entries from finger table
-spec(compress_finger_table(finger_table()) ->
             [{Index :: integer(), #node{}}]).

compress_finger_table(FT) ->
    lists:foldr(fun({{_, undefined}, _}, Acc) ->
                        Acc;
                   ({{_, Succ}, Idx}, Acc) ->
                        [{Idx, Succ} | Acc]
                end, [],
                lists:zip(FT, lists:seq(1, length(FT)))).

%% @doc Stabilizer function
%% Run periodically to check if new node appeared between current node and its
%% successor.

stabilizer() ->
    Interval = hordad_lcf:get_var({hordad_ddb, stabilize_interval}),

    timer:sleep(Interval),

    Self = get_self(),
    Succ = get_successor(),

    {ok, Pred} = session(Succ, ?SERVICE_TAG, "get_predecessor"),

    if
        Pred == undefined orelse Pred == Succ orelse Pred == Self ->
            ok;
        true ->
            case hordad_ddb_lib:between_right_inc(Self#node.id, Succ#node.id,
                                                  Pred#node.id) of
                true ->
                    hordad_log:info(?MODULE, "Stabilizer found new successor: "
                                    "~p:~p (~p)",
                                    [Pred#node.ip, Pred#node.port,
                                     Pred#node.id_str]),

                    set_successor(Pred);
                _ ->
                    ok
            end
    end,

    {ok, ok} = session(get_successor(), ?SERVICE_TAG, {"notify", Self}),

    stabilizer().

%% @doc Finger checker function
%% Run periodically to repair finger table

finger_checker() ->
    Interval = hordad_lcf:get_var({hordad_ddb, finger_checker_interval}),
    Self = get_self(),
    FT = get_finger_table(),
    Ids = [Id || {Id, _} <- FT],

    finger_checker(Self, Ids, Interval).

finger_checker(_, [], _) ->
    finger_checker();
finger_checker(Node, Ids, Interval) ->
    timer:sleep(Interval),

    case collect_successors(Node, Ids) of
        {error, Reason, Node, Rest, Found} ->
            hordad_log:error(?MODULE, "Error updating finger table: ~p",
                             [Reason]),

            %% Update with values found so far
            ok = update_finger_table(Found),

            %% Continue with those not found
            Int = hordad_lcf:get_var({hordad_ddb,
                                      finger_checker_retry_interval}),
            finger_checker(Node, Rest, Int);
        Fingers ->
            ok = update_finger_table(Fingers),
            finger_checker()
    end.

%% @doc Simple session wrapper
session(#node{ip=IP, port=Port}, Tag, Service) ->
    Timeout = hordad_lcf:get_var({hordad_ddb, net_timeout}),

    hordad_lib_net:gen_session(?MODULE, IP, Port, Tag, Service, Timeout).

%% @doc Check for possible predecessor change
-spec(do_notify(#node{}, #state{}) -> #state{}).

do_notify(Node, #state{predecessor=Pred, self=Self}=State) ->
    LogF = fun(#node{ip=IP, port=Port, id_str=Id}) ->
                   hordad_log:info(?MODULE, "New predecessor: "
                                   "~p:~p (~p)", [IP, Port, Id])
           end,

    if
        %% Its ourselves, ignore.
        Node == Self ->
            State;
        %% Our current predecessor
        Node == Pred ->
            State;
        %% New node recently joined
        Pred == undefined ->
            LogF(Node),
            State#state{predecessor=Node};
        true ->
            InRange = hordad_ddb_lib:between_right_inc(
                        Pred#node.id, Self#node.id, Node#node.id),

            if
                InRange == true ->
                    LogF(Node),
                    State#state{predecessor=Node};
                true ->
                    State
            end
    end.

%% @doc Workhouse for set_successor/1
-spec(do_set_successor(#node{}, #state{}) -> #state{}).

do_set_successor(Node, State) ->
    State#state{successor=Node}.

%% @doc Notify succesor of our arrival
notify_successor(Succ, Self) ->
    case session(Succ, ?SERVICE_TAG, {"join", Self}) of
        {error, E} ->
            hordad_log:warning(?MODULE, "Error notifying successor: ~p. "
                               "Retrying.", [E]),
            timer:sleep(?ERROR_RETRY_INTERVAL),

            notify_successor(Succ, Self);
        {ok, ok} ->
            ok
    end.
