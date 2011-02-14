%%% -------------------------------------------------------------------
%%% File    : hordad_aes_poller
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Hordad AES poller
%%%
%%% Created : 2010-02-11 by Max E. Kuznecov <mek@mek.uz.ua>
%%% -------------------------------------------------------------------

-module(hordad_aes_poller).

-behaviour(gen_server).

%% API
-export([start_link/0,
         add_nodes/2,
         clear_nodes/1,
         get_nodes/0,
         get_nodes/1,
         get_node_tags/1,
         get_reports/1,
         get_report/2,
         get_report_by_node/1,
         get_default_tag/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([service_handler/2]).
-export([get_ldb_tables/0]).

-include_lib("hordad_lib/include/lib_types.hrl").
-include_lib("hordad_aes_agent/include/hordad_aes_agent.hrl").

-define(SERVER, ?MODULE).
-define(SERVICE_TAG, "aes_poller").
-define(ROOM, ?MODULE).
-define(TABLE_NODES, poller_node).
-define(TABLE_TAGS, poller_tags).
-define(TAG_DEFAULT, "default").

-type(tag() :: string()).
-type(net_port() :: integer()).
-type(net_node() :: {ip_address(), net_port()}).
-type(report() :: {net_node(), #agent_report{}, integer()}).

-record(poller_node, {node, report, timestamp}).
-record(poller_tags, {node, tags}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Add nodes into polling queue with tag.
%% If node already in the queue it is ignored
%% @end
-spec(add_nodes(tag(), [net_node()]) -> ok | {error, any()}).

add_nodes(Tag, Nodes) ->
    try
        ToInsert = lists:map(
          fun(Node) ->
                  case hordad_ldb:read(?TABLE_NODES, Node, undefined) of
                      {ok, undefined} ->
                          [#poller_node{node=Node,
                                        report=#agent_report{},
                                        timestamp=now()},
                           [#poller_tags{node=Node, tags=[Tag]}]];
                      %% Node exists, just update tags
                      {ok, _} ->
                          {ok, Tags} = get_node_tags(Node),

                          #poller_tags{node=Node,
                                       tags=lists:usort([Tag | Tags])}
                  end
          end, Nodes),

        hordad_ldb:write(lists:flatten(ToInsert))
    catch
        _:E ->
            {error, E}
    end.

%% @doc Return list of node tags
-spec(get_node_tags(net_node()) -> {ok, [tag()]} | {error, any()}).

get_node_tags(Node) ->
    case hordad_ldb:read(?TABLE_TAGS, Node, undefined) of
        {ok, undefined} ->
            {ok, []};
        {ok, [#poller_tags{tags=Tags}]} ->
            {ok, Tags};
        E ->
            E
    end.

%% @doc Remove all nodes with provided tag
-spec(clear_nodes(tag()) -> ok | {error, any()}).

clear_nodes(Tag) ->
    Nodes = get_nodes(Tag),

    Delete = lists:flatten(
               lists:map(
                 fun(Node) ->
                         case get_node_tags(Node) of
                             %% Node with only tag, remove completely
                             {ok, [Tag]} ->
                                 [{?TABLE_NODES, Node}, {?TABLE_TAGS, Node}];
                             %% Node has more tags, just remove provided one
                             {ok, Tags} ->
                                 hordad_ldb:write(
                                   #poller_tags{node=Node, tags=Tags--[Tag]}),

                                 []
                         end
                 end, Nodes)),

    hordad_ldb:delete(Delete).

%% @doc Get all nodes
-spec(get_nodes() -> [net_node()]).

get_nodes() ->
    {ok, Nodes} = hordad_ldb:all_keys(?TABLE_NODES),

    Nodes.

%% @doc Get nodes with provided tag
-spec(get_nodes(tag()) -> [net_node()]).

get_nodes(Tag) ->
    {ok, Nodes} = 
        hordad_ldb:foldl(fun(#poller_tags{tags=Tags, node=Node}, Acc) ->
                                 case lists:member(Tag, Tags) of
                                     true ->
                                         [Node | Acc];
                                     false ->
                                         Acc
                                 end
                         end, [], ?TABLE_TAGS),

    lists:sort(Nodes).

%% @doc Get reports of all nodes with provided tag
-spec(get_reports(tag()) -> [report()]).

get_reports(Tag) ->
    {ok, Reports} = hordad_ldb:foldl(
                      fun(#poller_node{node=Node,
                                       report=Report,
                                       timestamp=TS}, Acc) ->
                              {ok, NodeTags} = get_node_tags(Node),

                              case lists:member(Tag, NodeTags) of
                                  true ->
                                      [{Node, Report, TS} | Acc];
                                  false ->
                                      Acc
                              end
                      end, [], ?TABLE_NODES),

    Reports.

%% @doc Return report for proivded tag and node
-spec(get_report(tag(), net_node()) -> report() | undefined).

get_report(Tag, Node) ->
    case lists:keyfind(Node, 1, get_reports(Tag)) of
        false ->
            undefined;
        Report ->
            Report
    end.
    
%% @doc Get report by provided node
-spec(get_report_by_node(net_node()) -> report() | undefined).

get_report_by_node(Node) ->
    case hordad_ldb:match(#poller_node{node=Node, _='_'}) of
        {ok, [Report | _]} ->
            Report;
        _ ->
            undefined
    end.

%% @doc Return default tag
-spec(get_default_tag() -> tag()).

get_default_tag() ->
    ?TAG_DEFAULT.

%%====================================================================
%% gen_server callbacks
%%====================================================================
service_handler({"poll", Ref}, _Socket) ->
    Reports = get_reports(?TAG_DEFAULT),

    {ok, Ref, Reports}.

%% @doc hordad_ldb table info callback.
-spec(get_ldb_tables() -> [{Name :: atom(), Attrs :: [{atom(), any()}]}]).

get_ldb_tables() ->
    [
     {?TABLE_NODES, [{attributes, record_info(fields, poller_node)}]},
     {?TABLE_TAGS, [{attributes, record_info(fields, poller_tags)}]}
    ].

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

    hordad_rooms:create(?ROOM),

    %% Add nodes to default tag
    ok = add_nodes(?TAG_DEFAULT,
                   hordad_lcf:get_var({hordad_aes_poller, nodes},[])),

    {ok, init_poller()}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
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
handle_info({'DOWN', Ref, process, Pid, Info}, {Pid, Ref}) ->
    hordad_log:warning(?MODULE, "Poller process died: ~p. Restarting",
                       [Info]),

    {noreply, init_poller()};
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

%% @doc Init poller
-spec(init_poller() -> {pid(), reference()}).

init_poller() ->
    spawn_monitor(fun poller/0).

%% @doc Periodically poll nodes for physical availability
poller() ->
    timer:sleep(hordad_lcf:get_var({hordad_aes_poller, interval})),

    Self = self(),
    Poller = spawn(fun() -> poller_engine(Self) end),

    receive
        {Poller, Report} ->
            save_report(Report);
        Unexpected ->
            hordad_log:error(?MODULE, "Unexpected poller report: ~9999p",
                             [Unexpected])
    end,

    poller().

%% @doc Poller engine
poller_engine(Parent) ->
    Nodes = get_nodes(),
    Me = self(),
    Ref = make_ref(),
    Data = dict:from_list([{X, #agent_report{status=down}} || X <- Nodes]),

    Spawned = lists:map(
                fun(Node) ->
                        spawn(fun() -> poll_node(Me, Ref, Node) end)
                end, Nodes),

    Report = poller_loop(Ref, Data, Spawned),

    Parent ! {Me, dict:to_list(Report)}.

%% @doc Poller loop
poller_loop(_Ref, Data, []) ->
    Data;
poller_loop(Ref, Data, Spawned) ->
    {DataNew, Rest} = 
        receive
            {Ref, Pid, Node, Report} ->
                {dict:store(Node, Report, Data), Spawned -- [Pid]};
            {Ref, Pid, error} ->
                {Data, Spawned -- [Pid]}
        end,

    case Rest of
        %% All ready
        [] ->
            DataNew;
        %% More to come
        _ ->
            poller_loop(Ref, DataNew, Rest)
    end.
   
%% @doc Poll node and send result to a parent
poll_node(Parent, Ref, {IP, Port}=Node) ->
    Timeout = hordad_lcf:get_var({?MODULE, poll_timeout}),
    hordad_log:debug(?MODULE, "Polling node: ~p:~p", [IP, Port]),

    try
        {ok, Report} = hordad_lib_net:gen_session(?MODULE, IP, Port,
                                                  "aes_agent", "report",
                                                  Timeout),
        hordad_log:debug(?MODULE, "Node ~p:~p status=~p, lar=~p",
                         [IP, Port, Report#agent_report.status,
                          Report#agent_report.lar]),

        Parent ! {Ref, self(), Node, Report}
    catch
        _:_ ->
            hordad_log:error(?MODULE, "Error polling node ~p: ~9999p",
                             [Node, erlang:get_stacktrace()]),
            Parent ! {Ref, self(), error}
    end.

%% @doc Update nodes reports in db
save_report(Reports) ->
    TS = now(),

    ToSave = 
        lists:map(
          fun({Node, #agent_report{status=Status, lar=Lar}=NewReport}) ->
                  case get_report_by_node(Node) of
                      undefined ->
                          #poller_node{node=Node, report=NewReport,
                                       timestamp=TS};
                      %% Check for changed values
                      #poller_node{report=#agent_report{status=OldStatus,
                                                         lar=OldLar}}=Entry->
                          if
                              Status /= OldStatus ->
                                  notify_room(status_changed, Node, OldStatus,
                                              Status, TS);
                              true ->
                                  ok
                          end,

                          if
                              Lar /= OldLar ->
                                  notify_room(lar_changed, Node, OldLar,
                                              Lar, TS);
                              true ->
                                  ok
                          end,

                          Entry#poller_node{report=NewReport, timestamp=TS}
                  end
          end, Reports),
    
    hordad_ldb:write(ToSave).

%% @doc Notify room
notify_room(Tag, Node, Old, New, TS) ->
    hordad_rooms:send(?ROOM, {?MODULE, Tag, Node, Old, New, TS}).
