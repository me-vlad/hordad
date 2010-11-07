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
         get_successor/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([service_handler/2]).

-include("hordad_ddb_lookup.hrl").

-define(SERVER, ?MODULE).
-define(SERVICE_TAG, "ddb_lookup").

-record(state, {
          node,
          successor,
          predecessor,
          finger_table,
          successor_list,
          stabilizer
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
-spec(set_successor(#node{}) -> ok).

set_successor(Node) when is_record(Node, node) ->
    gen_server:call(?SERVER, {set_successor, Node}).

%% @doc Find successor for provided Id
-spec(get_successor(integer()) -> {ok, #node{}} | {next, #node{}}).

get_successor(Id) ->
    gen_server:call(?SERVER, {get_successor, Id}).

%% @doc Service handler callback
service_handler({"find_successor", Id}, _Socket) ->
    get_successor(Id).

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

    Node = hordad_ddb_lib:make_node(hordad_lcf:get_var({hordad_ddb, node_ip})),

    case hordad_lcf:get_var({hordad_ddb, entry_point}, undefined) of
        undefined ->
            hordad_log:info(?MODULE,
                            "No entry point defined. New network created",
                            []),
            ok;
        Entry ->
            spawn(fun() -> join(Entry, Node) end)
    end,

    {ok, #state{
       node = Node,
       successor = undefined,
       predecessor = undefined,
       successor_list = [],
       finger_table = [],
       stabilizer = init_stabilizer()
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
handle_call({get_successor, Id}, _From, State) ->
    {reply, do_get_successor(Id, State), State}.

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
    Timeout = hordad_lcf:get_var({hordad_ddb, net_timeout}),

    hordad_log:info(?MODULE, "Trying to join existing overlay network using ~p"
                    "as entry point", [Entry]),

    case hordad_lib_net:gen_session(?MODULE, Entry, ?SERVICE_TAG,
                                    {"find_successor", Node#node.id},
                                    Timeout) of
        {error, E} ->
            hordad_log:error(?MODULE, "Unable to join overlay network: ~p",
                             [E]),
                    
            timer:sleep(5000),

            join(Entry, Node);
        {ok, {next, Next}} ->
            join(Next#node.ip, Node);
        {ok, {ok, Succ}} ->
            hordad_log:info(?MODULE, "Found successor: ~p", [Succ]),

            set_successor(Succ)
    end.

%% @doc Workhouse for get_successor/1
do_get_successor(Id, #state{node=Self, successor=Succ}=State) ->
    case hordad_ddb_lib:is_node_in_range(Self#node.id, Succ#node.id, Id) of
        % Found successor
        true ->
            {ok, Succ};
        % Search in finger table
        false ->
            {next, closest_preceding_node(Id, State)}
    end.

%% @doc Find the closest preceding node according to finger table info
-spec(closest_preceding_node(integer(), #state{}) -> #node{}).

closest_preceding_node(Id, #state{node=Self, finger_table=FT}) ->
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

%% @doc Init stabilizer process
init_stabilizer() ->
    erlang:spawn_monitor(fun stabilizer/0).
    
%% @doc Stabilizer function
stabilizer() ->
    stabilizer().
