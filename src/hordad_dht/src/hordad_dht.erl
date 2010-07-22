%%% -------------------------------------------------------------------
%%% File    : hordad_dht
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Hordad DHT module
%%%
%%% Created : 2010-02-03 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_dht).

-behaviour(gen_server).

-include("hordad_dht.hrl").

%% API
-export([start_link/0,
         get_async/2,
         get_sync/1,
         set_async/3,
         set_sync/2
        ]).

%% Internal stuff
-export([service_handler/2,
         route/4,
         request_watcher/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVICE_TAG, "dht").
-define(SERVER, ?MODULE).

-define(is_callback(CB), is_pid(CB) orelse
                         is_function(CB) orelse
                         (is_tuple(CB) andalso
                          tuple_size(CB) == 3 andalso
                          is_atom(element(1, CB)) andalso
                          is_atom(element(2, CB)) andalso
                          is_list(element(3, CB)))).

-type(callback() :: pid() | function() | tuple()).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Get key value. The function is asynchronous,
%%      the CB argument can be either pid() or fun() or
%%      tuple in format {Module, Fun, Args}.
%%      The callback will be passed and argument/sent a message in form:
%%      {get_result, Value} or {error, Reason}
-spec(get_async(any(), callback()) -> ok).

get_async(Key, CB) when ?is_callback(CB) ->
    gen_server:call(?SERVER, {get, Key, CB}).

%% @doc Synchronous version of get
-spec(get_sync(any()) -> {ok, any()} | {error, any()}).

get_sync(Key) ->
    Self = self(),

    F = fun() ->
                get_async(Key, Self)
        end,

    spawn(F),

    receive
        {get_result, Value} ->
            {ok, Value};
        {error, _}=E ->
            E
    end.

%% @doc Set key value asynchronously. See get_async/2 description.
-spec(set_async(any(), any(), callback()) -> ok).

set_async(Key, Value, CB) when ?is_callback(CB) ->
    gen_server:call(?SERVER, {set, Key, Value, CB}).

%% @doc Synchronous version of set
-spec(set_sync(any(), any()) -> {ok, any()} | {error, any()}).

set_sync(Key, Value) ->
    Self = self(),

    F = fun() ->
                set_async(Key, Value, Self)
        end,

    spawn(F),

    receive
        {set_result, Value} ->
            {ok, Value};
        {error, _}=E ->
            E
    end.

%% @doc Deliver a message to this very node
deliver(Msg, Key, Ref, IP) ->
    gen_server:call(?SERVER, {deliver, Msg, Key, Ref, IP}).

%% @doc Perform request completion
complete_request(Msg) ->
    gen_server:call(?SERVER, {complete_request, Msg}).

%% ---------------------------------------------

%% @doc Handle incoming service requests
-spec(service_handler(any(), port()) -> ok).

service_handler({route, Msg, Key, Ref, IP}, _Socket) ->
    route(Msg, Key, Ref, IP);
service_handler({get_result, _Val, _Ref}=Msg, _Socket) ->
    complete_request(Msg);
service_handler({set_result, _Val, _Ref}=Msg, _Socket) ->
    complete_request(Msg).

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

handle_call({get, Key, CB}, _From, State) ->
    {reply, ok, do_init_request(get, Key, CB, State)};
handle_call({set, Key, Value, CB}, _From, State) ->
    {reply, ok, do_init_request({set, Value}, Key, CB, State)};
handle_call({deliver, Msg, Key, Ref, IP}, _From, State) ->
    {reply, do_deliver(Msg, Key, Ref, IP), State};
handle_call({complete_request, Msg}, _From, State) ->
    {reply, ok, do_complete_request(Msg, State)}.

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
do_init_request(Msg, Key, CB, State) ->
    % 1. Store request in dict
    Ref = make_ref(),
    IP = hordad_lcf:get_var({hordad_dht, node_ip}),

    % 2. Spawn watcher proc
    Watcher = spawn(?MODULE, request_watcher, [Ref]),

    % 3. Spawn routing proc
    spawn(?MODULE, route, [Msg, Key, Ref, IP]),

    dict:store(Ref, {CB, Watcher}, State).

%% @doc Perform request completion
-spec(do_complete_request(any(), dict()) -> dict()).

do_complete_request(Msg, Dict) ->
    {Ref, Val} = case Msg of
                     {get_result, Value, RefOrig} ->
                         {RefOrig, {get_result, Value}};
                     {set_result, Value, RefOrig} ->
                         {RefOrig, {set_result, Value}};
                     {error, Reason, RefOrig} ->
                         {RefOrig, {error, Reason}};
                     {timeout, RefOrig} ->
                         {RefOrig, {error, timeout}};
                     _ ->
                         hordad_log:warning(
                           ?MODULE,
                           "Unable to complete request ~9999p:"
                           "invalid request", [Msg]),
                         
                         {invalid, undefined}
                 end,

    %% Locate stored request
    case dict:find(Ref, Dict) of
        {ok, {CB, Watcher}} ->
            hordad_log:info(?MODULE,
                             "~p: completing request: ~9999p", [Ref, Val]),

            Watcher ! completed,
            run_callback(CB, Val),

            dict:erase(Ref, Dict);
        erorr ->
            hordad_log:warning(?MODULE,
                               "Unable to complete request - "
                               "not in queue: ~9999p.", [Msg]),
            Dict
    end.

%% @doc Request watcher
request_watcher(Ref) ->
    Timeout = hordad_lcf:get_var({hordad_dht, net_timeout}),

    % Ensure request has completed
    receive
        completed ->
            ok
    after
        Timeout ->
            complete_request({timeout, Ref})
    end.

%% @doc Run callback depending on its type.
-spec(run_callback(callback(), any()) -> ok).

run_callback(CB, Value) when is_pid(CB) ->
    CB ! Value,
    ok;
run_callback(CB, Value) when is_function(CB) ->
    CB(Value),
    ok;
run_callback({Module, Fun, Args}, Value) ->
    Module:Fun([Value | Args]),
    ok.

%% @doc Check if node id is already set and if not, set it
-spec(set_node_id() -> ok).

set_node_id() ->
    case hordad_lcf:get_var({hordad_dht, node_id}, undefined) of
        undefined ->
            NodeId = case hordad_dht_meta:lookup(node_id) of
                         [Id] ->
                             Id;
                         undefined ->
                             % No ID yet, generate new one
                             Id = hordad_dht_lib:gen_id(
                                    hordad_lcf:get_var({hordad_dht, node_ip})),
                             hordad_dht_meta:insert({node_id, Id}),

                             Id
                     end,

            hordad_lcf:set_var({hordad_dht, node_id}, NodeId),
            hordad_lcf:set_var({hordad_dht, node},
                               #dht_node{
                                 id=NodeId,
                                 id_num=hordad_dht_lib:id_str2num(NodeId),
                                 ip=hordad_lcf:get_var({hordad_dht, node_ip})})
            
    end,

    ok.

%% @doc Send a message to remote node
send_engine(Msg, IP) ->
    Timeout = hordad_lcf:get_var({hordad_dht, net_timeout}),

    {ok, ok} = hordad_lib_net:gen_session(?MODULE, IP, ?SERVICE_TAG, Msg,
                                          Timeout),
    ok.
