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

%% API
-export([start_link/0,
         get_async/2,
         get_sync/1,
         deliver/3
        ]).

-export([service_handler/2, route/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVICE_TAG, "dht").
-define(SERVER, ?MODULE).

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
%%      the Pid argument will be sent a message in form:
%%      {get_result, Key, Value} or {error, Reason}
-spec(get_async(any(), pid()) -> ok).

get_async(Key, Pid) ->
    gen_server:call(?SERVER, {get, Key, Pid}).

%% @doc Synchronous version of get
-spec(get_sync(any()) -> {ok, any()} | {error, any()}).

get_sync(Key) ->
    Self = self(),

    F = fun() ->
                get_async(Key, Self)
        end,

    spawn(F),
    
    Timeout = hordad_lcf:get_var({hordad_dht, get_timeout}),

    receive
        {get_result, Key, Value} ->
            {ok, Value};
        {error, _}=E ->
            E
    after
        Timeout ->
            {error, timeout}
    end.

%% @doc Deliver a message to this very node
deliver(Msg, Id, Ref) ->
    gen_server:call(?SERVER, {deliver, Msg, Id, Ref}).

%% ---------------------------------------------

%% @doc Handle incoming service requests
-spec(service_handler(any(), port()) -> ok).

service_handler({route, Msg, Id, Ref}, _Socket) when is_reference(Ref) ->
    route(Msg, Id, Ref).

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

    % Node 
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

handle_call({get, Key, Pid}, _From, State) ->
    % 1. Store request in dict
    Ref = make_ref(),
    NewState = dict:store(Ref, Pid, State),

    % 2. Spawn routing proc
    spawn(?MODULE, route, [{get, Key}, Key, Ref]),
    
    {reply, ok, NewState};
handle_call({deliver, Msg, Id, Ref}, _From, State) ->
    do_deliver(Msg, Id, Ref, State).

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

%% @doc Select next node in routing chain
route(Msg, Id, Ref) ->
    % First check the leaf set
    case hordad_dht_leaf_set:has_node(Id) of
        % The destination node is the current one. Deliver to itself
        self ->
            deliver(Msg, Id, Ref);
        % We've got Id in our leaf set. Forward msg to it
        {has, NodeId} ->
            forward(Msg, Id, Ref, NodeId);
        % No node in leaf set, go on with routing
        false ->
            case hordad_dht_route_table:get_node(Id) of
                {ok, NodeId} ->
                    forward(Msg, Id, Ref, NodeId);
                undefined ->
                    rare_case
            end
    end.

%% @doc Join existing DHT
-spec(join_dht(IP :: tuple()) -> ok).

join_dht(EntryPoint) ->
    pass.

%% @doc Forward message to another node
forward(Msg, Id, Ref, NodeId) ->
    pass.

%% @doc Workhouse for deliver/3
do_deliver({get, Key}, Id, Ref, Dict) ->
    Val = hordad_storage:lookup(Key, undefined),

    Dict.
