%%% -------------------------------------------------------------------
%%% File    : hordad_dht
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Hordad DHT module
%%%
%%% Created : 2010-02-03
%%% @copyright 2009-2010 Max E. Kuznecov
%%% -------------------------------------------------------------------

-module(hordad_dht).

-behaviour(gen_server).

-include("hordad_dht.hrl").

%% API
-export([start_link/0,
         get/1,
         set/2
        ]).

%% Internal stuff
-export([service_handler/2,
         route/4
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([get_ldb_tables/0]).

-define(TABLE, dht_meta).
-define(SERVICE_TAG, "dht").
-define(SERVER, ?MODULE).

-record(dht_meta, {key, value}).

%% @doc hordad_ldb table info callback.
-spec(get_ldb_tables() -> {Name :: atom(), Attrs :: [{atom(), any()}]}).

get_ldb_tables() ->
    {?TABLE, [{attributes, record_info(fields, dht_meta)}]}.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Get key value.
-spec(get(any()) -> {error, timeout} | {error, any()} | any()).

get(Key) ->
    call({get, Key}).

%% @doc Set key value.
-spec(set(string(), any()) -> {error, timeout} | {error, any()} | any()).

set(Key, Value) ->
    call({set, Key, Value}).

%% @doc Deliver a message to this very node
deliver(Msg, Key, Ref, IP) ->
    gen_server:call(?SERVER, {deliver, Msg, Key, Ref, IP}).

%% @doc Perform request completion
-spec(complete_request(reference(), any()) -> ok).

complete_request(Ref, Val) ->
    gen_server:call(?SERVER, {complete_request, Ref, Val}).

%% ---------------------------------------------

%% @doc Handle incoming service requests
-spec(service_handler(any(), port()) -> ok).

service_handler({route, Msg, Key, Ref, IP}, _Socket) ->
    case notify(Msg, Key, Ref, IP) of
        continue ->
            route(Msg, Key, Ref, IP);
        stop ->
            ok
    end;
service_handler({get_result, Val, Ref}, _Socket) ->
    complete_request(Ref, Val);
service_handler({set_result, Val, Ref}, _Socket) ->
    complete_request(Ref, Val).

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

    set_node_id(),

    % Check if entry point supplied
    ok = case hordad_lcf:get_var({hordad_dht, entry_point}, undefined) of
             undefined ->
                 ok;
             EntryPoint ->
                 join_dht(EntryPoint)
         end,

    {ok, dict:new()}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({{get, Key}, Ref}, From, State) ->
    {noreply, do_init_request(get, Key, Ref, From, State)};

handle_call({{set, Key, Value}, Ref}, From, State) ->
    {noreply, do_init_request({set, Value}, Key, Ref, From, State)};

handle_call({{join, Key}, Ref}, From, State) ->
    {noreply, do_init_request(join, Key, Ref, From, State)};

handle_call({deliver, Msg, Key, Ref, IP}, _From, State) ->
    {reply, do_deliver(Msg, Key, Ref, IP), State};

handle_call({cancel, Ref}, _From, State) ->
    {reply, dict:erase(Ref, State)};

handle_call({complete_request, Ref, Val}, _From, State) ->
    NewState= do_complete_request(Ref, Val, State),

    {reply, ok, NewState}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
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

%% @doc Select next node in routing chain
route(Msg, Key, Ref, IP) ->
    hordad_log:info(?MODULE, "~p: routing message: ~9999p from ~p",
                    [Ref, Msg, IP]),

    % First check the leaf set
    case hordad_dht_leaf_set:has_node(Key) of
        % The destination node is the current one. Deliver to itself
        self ->
            hordad_log:info(?MODULE, "~p: delivering to self", [Ref]),

            deliver(Msg, Key, Ref, IP);
        % We've got Id in our leaf set. Forward msg to it
        {has, Next} ->
            hordad_log:info(?MODULE, "~p: forwarding to ~p (leaf)",
                            [Ref, Next#dht_node.ip]),

            forward(Msg, Key, Ref, Next, IP);
        % No node in leaf set, go on with routing
        false ->
            case hordad_dht_route_table:get_node(Key) of
                {error,  Reason} ->
                    hordad_log:error(?MODULE,
                                     "Error checking route table: ~9999p",
                                     [Reason]),
                    ok;
                {ok, Next} ->
                    hordad_log:info(?MODULE, "~p: forwarding to ~p (route)",
                                    [Ref, Next#dht_node.ip]),

                    forward(Msg, Key, Ref, Next, IP);
                undefined ->
                    Node = hordad_lcf:get_var({hordad_dht, node}),
                    Prefix = hordad_dht_lib:shared_prefix(Key,
                                                          Node#dht_node.id),
                    KeyNum = hordad_dht_lib:id_str2num(Key),

                    Ids = hordad_dht_leaf_set:get_all_nodes() ++
                        hordad_dht_route_table:get_all_nodes() ++
                        hordad_dht_neighborhood_set:get_all_nodes(),

                    % Check for the first node with prefix equal to
                    % one of current node but numerically greater
                    Next = lists:foldl(
                             fun(#dht_node{id=Id, id_num=IdNum}=Entry, Acc) ->
                                     P = hordad_dht_lib:shared_prefix(Id, Key),

                                     if
                                         P >= Prefix andalso
                                         abs(IdNum - KeyNum) <
                                         abs(Node#dht_node.id_num - KeyNum) ->
                                             Entry;
                                         true ->
                                             Acc
                                     end
                             end, Node, Ids),

                    forward(Msg, Key, Ref, Next, IP)
            end
    end.

%% @doc Join existing DHT
-spec(join_dht(IP :: tuple()) -> ok).

join_dht(EntryPoint) ->
    Node = hordad_lcf:get_var({hordad_dht, node}),
    ok.

%% @doc Forward message to another node
forward(Msg, Key, Ref, #dht_node{ip=NextIP}, IP) ->
    send_engine({route, Msg, Key, Ref, IP}, NextIP).

%% @doc Workhouse for deliver/4
do_deliver(get, Key, Ref, IP) ->
    {ok, Val} = hordad_dht_storage:lookup(Key, undefined),
    send_engine({get_result, Val, Ref}, IP);

do_deliver({set, Value}, Key, Ref, IP) ->
    ok = hordad_dht_storage:insert(Key, Value),
    send_engine({set_result, ok, Ref}, IP).

%% @doc Init new request
-spec(do_init_request(any(), string(), reference(), tuple(), dict())
      -> dict()).

do_init_request(Msg, Key, Ref, From, State) ->
    IP = hordad_lcf:get_var({hordad_dht, node_ip}),

    spawn(?MODULE, route, [Msg, Key, Ref, IP]),

    dict:store(Ref, From, State).

%% @doc Perform request completion
-spec(do_complete_request(reference(), any(), dict()) -> dict()).

do_complete_request(Ref, Val, State) ->
    %% Locate stored request
    case dict:find(Ref, State) of
        {ok, Client} ->
            hordad_log:info(?MODULE,
                             "~p: completing request: ~9999p", [Ref, Val]),

            gen_server:reply(Client, Val),

            dict:erase(Ref, State);
        error ->
            hordad_log:warning(?MODULE,
                               "Unable to complete request - "
                               "not in queue: ~9999p.", [Ref]),
            State
    end.

%% @doc Check if node id is already set and if not, set it
-spec(set_node_id() -> ok).

set_node_id() ->
    NodeId = case hordad_ldb:read(?TABLE, node_id, undefined) of
                 {ok, undefined} ->
                     % No ID yet, generate new one
                     Id = hordad_dht_lib:gen_id(
                            hordad_lcf:get_var({hordad_dht, node_ip})),
                     Id;
                 {ok, [#dht_meta{value=Id}]} ->
                     Id
             end,

    hordad_ldb:write(#dht_meta{key=node_id, value=NodeId}),
    hordad_lcf:set_var({hordad_dht, node_id}, NodeId),
    hordad_lcf:set_var({hordad_dht, node},
                       #dht_node{
                         id=NodeId,
                         id_num=hordad_dht_lib:id_str2num(NodeId),
                         ip=hordad_lcf:get_var({hordad_dht, node_ip})}),

    ok.

%% @doc Send a message to remote node
send_engine(Msg, IP) ->
    Timeout = hordad_lcf:get_var({hordad_dht, net_timeout}),

    {ok, ok} = hordad_lib_net:gen_session(?MODULE, IP, ?SERVICE_TAG, Msg,
                                          Timeout),
    ok.

%% @doc General function for calling DHT API
call(Msg) ->
    Timeout = hordad_lcf:get_var({hordad_dht, net_timeout}),
    Ref = make_ref(),

    try
       gen_server:call(?SERVER, {Msg, Ref}, Timeout)
    catch
        {exit, {timeout, _}} ->
            gen_server:call(?SERVER, {cancel, Ref}),

            {error, timeout};
        _:E ->
            gen_server:call(?SERVER, {cancel, Ref}),

            {error, E}
    end.

notify(join, Kef, Ref, IP) ->
    continue;
notify(Msg, Kef, Ref, IP) ->
    continue.
