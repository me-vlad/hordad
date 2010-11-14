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
         find_successor/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([service_handler/2]).

-include("hordad_ddb_lookup.hrl").

-define(SERVER, ?MODULE).
-define(SERVICE_TAG, "ddb_lookup").

-record(state, {
          self,
          successor,
          predecessor,
          finger_table,
          successor_list,
          stabilizer,
          predecessor_checker
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
-spec(get_successor() -> #node{} | undefined).

get_successor() ->
    gen_server:call(?SERVER, get_successor).

%% @doc Get current node's predecessor
-spec(get_predecessor() -> #node{} | undefined).

get_predecessor() ->
    gen_server:call(?SERVER, get_predecessor).

%% @doc Find successor for provided Id
-spec(find_successor(integer()) -> {ok, #node{}} | {next, #node{}}).

find_successor(Id) ->
    gen_server:call(?SERVER, {find_successor, Id}).

%% @doc Service handler callback
service_handler({"find_successor", Id}, _Socket) ->
    find_successor(Id);
service_handler("get_predecessor", _Socket) ->
    get_predecessor();
service_handler({"pred_change", Node}, _Socket) ->
    gen_server:call(?SERVER, {pred_change, Node}),
    ok.

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

    Self = hordad_ddb_lib:make_node(hordad_lcf:get_var({hordad_ddb, node_ip})),

    case hordad_lcf:get_var({hordad_ddb, entry_point}, undefined) of
        undefined ->
            hordad_log:info(?MODULE,
                            "No entry point defined. New network created",
                            []),
            ok;
        Entry ->
            spawn(fun() -> join(Entry, Self) end)
    end,

    {ok, #state{
       self = Self,
       successor = undefined,
       predecessor = undefined,
       successor_list = [],
       finger_table = [],
       stabilizer = init_stabilizer(),
       predecessor_checker = init_predecessor_checker()
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
    {reply, ok, State#state{successor=Node}};
handle_call({set_predecessor, Node}, _From, State) ->
    {reply, ok, State#state{predecessor=Node}};
handle_call(get_self, _From, #state{self=Self}=State) ->
    {reply, Self, State};
handle_call(get_successor, _From, #state{successor=Succ}=State) ->
    {reply, Succ, State};
handle_call(get_predecessor, _From, #state{predecessor=Pred}=State) ->
    {reply, Pred, State};
handle_call({pred_change, Node}, _From, State) ->
    {reply, ok, do_pred_change(Node, State)};
handle_call({find_successor, Id}, _From, State) ->
    {reply, do_find_successor(Id, State), State}.

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
handle_info(Msg, State) ->
    hordad_log:warning(?MODULE, "Unknown message received: ~9999p", [Msg]),

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

%% @doc Join network
join(Entry, Node) ->
    hordad_log:info(?MODULE, "Trying to join existing overlay network using ~p"
                    "as entry point", [Entry]),

    case session(Entry, ?SERVICE_TAG, {"find_successor", Node#node.id}) of
        {error, E} ->
            hordad_log:error(?MODULE, "Unable to join overlay network: ~p",
                             [E]),
                    
            timer:sleep(5000),

            join(Entry, Node);
        {ok, {next, Next}} ->
            join(Next#node.ip, Node);
        {ok, {successor, Succ}} ->
            hordad_log:info(?MODULE, "Found successor: ~p", [Succ]),

            set_successor(Succ)
    end.

%% @doc Workhouse for find_successor/1
do_find_successor(Id, #state{self=Self, successor=Succ}=State) ->
    case hordad_ddb_lib:is_node_in_range(Self#node.id, Succ#node.id, Id) of
        % Found successor
        true ->
            {successor, Succ};
        % Search in finger table
        false ->
            {next, closest_preceding_node(Id, State)}
    end.

%% @doc Find the closest preceding node according to finger table info
-spec(closest_preceding_node(integer(), #state{}) -> #node{}).

closest_preceding_node(Id, #state{self=Self, finger_table=FT}) ->
    find_preceding_node(Id, Self#node.id, FT, Self).

find_preceding_node(_, _, [], Def) ->
    Def;
find_preceding_node(Id, SelfId, [Node | T], Def) ->
    case hordad_ddb_lib:is_node_in_range(SelfId, Id, Node#node.id) of
        true ->
            Node;
        _ ->
            find_preceding_node(Id, SelfId, T, Def)
    end.

%% @doc Init pred checker process
init_predecessor_checker() ->
    erlang:spawn_monitor(fun predecessor_checker/0).

%% @doc Init stabilizer process
init_stabilizer() ->
    erlang:spawn_monitor(fun stabilizer/0).
    
%% @doc Stabilizer function
%% Run periodically to check if new node appeared between current node and its
%% successor.

stabilizer() ->
    Interval = hordad_lcf:get_var({hordad_ddb, stabilize_interval}),

    timer:sleep(Interval),

    Self = get_self(),

    case get_successor() of
        undefined ->
            ok;
        Succ ->
            {ok, Pred} = session(Succ#node.ip, ?SERVICE_TAG,
                                 "get_predecessor"),

            case hordad_ddb_lib:is_node_in_range(Self#node.id, Succ#node.id, 
                                                 Pred#node.id) of
                true ->
                    {ok, ok} = session(Pred#node.ip, ?SERVICE_TAG,
                                       {"pred_change", Self}),

                    hordad_log:info(?MODULE,
                                    "Stabilizer found new successor: ~p",
                                    [Pred]),

                    set_successor(Pred);
                false ->
                    ok
            end
    end,

    stabilizer().

%% @doc Predecessor checker function
%% Run periodically to check if our predecessor has failed.

predecessor_checker() ->
    Interval = hordad_lcf:get_var({hordad_ddb, pred_checker_interval}),

    timer:sleep(Interval),

    case get_predecessor() of
        undefined ->
            ok;
        Pred ->
            case session(Pred#node.ip, "aes_agent", "status") of
                {ok, available} ->
                    ok;
                %% Assume failed
                _ ->
                    hordad_log:info(?MODULE, "Predecessor is down", []),

                    set_predecessor(undefined)
            end
    end,

    predecessor_checker().

%% @doc Simple session wrapper
session(IP, Tag, Service) ->
    Timeout = hordad_lcf:get_var({hordad_ddb, net_timeout}),

    hordad_lib_net:gen_session(?MODULE, IP, Tag, Service, Timeout).

%% @doc Check for possible predecessor change
-spec(do_pred_change(#node{}, #state{}) -> #state{}).

do_pred_change(Node, #state{predecessor=Pred, self=Self}=State) ->
    if
        Pred == undefined ->
            State#state{predecessor=Node};
        true ->
            InRange = hordad_ddb_lib:is_node_in_range(
                        Pred#node.id,
                        Self#node.id,
                        Node#node.id),

            case InRange of
                true ->
                    State#state{predecessor=Node};
                false ->
                    State
            end
    end.
    
