%%% -------------------------------------------------------------------
%%% File    : hordad_dht_srv
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Hordad DHT server
%%%
%%% Created : 2010-06-08 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------
-module(hordad_dht_srv).

-behaviour(gen_server).

%% Server API
-export([start_link/0,
         service_handler/2
        ]).

%% Client API
-export([get/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(SERVICE_TAG, "dht").

-record(state, {meta,
                storage,
                route,
                leaf,
                nb,
                requests
               }).

-record(request, {
          ref,
          pid
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

%% @doc Handle incoming service requests
-spec(service_handler(any(), port()) -> ok).

service_handler(Msg, _ClientSocket) ->
    gen_server:cast(?SERVER, {service_handler, Msg}).

%% @doc Get key value. The function is asynchronous,
%%      the Pid argument will be sent a message in form:
%%      {get_result, Key, Value} or {error, Reason} upon receiving response
-spec(get(string(), pid()) -> ok).

get(Key, Pid) ->
    gen_server:cast(?SERVER, {get, Key, Pid}).

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
    try
        {ok, _State} = R = init_state(),
        hordad_registrar:register(?SERVICE_TAG,
                                  {hordad_service, generic_service_handler,
                                   [?MODULE, service_handler, []]}),

        % Check if entry point supplied
        case hordad_lcf:get_var({hordad_dht, entry_point}, undefined) of
            undefined ->
                R;
            EntryPoint ->
                join_dht(EntryPoint),

                R
        end
    catch
        _:E ->
            hordad_log:error(?MODULE, "Error in init: ~9999p (~9999p)",
                             [E, erlang:get_stacktrace()]),
            {stop, E}
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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({service_handler, Msg}, State) ->
    {noreply, do_service_handler(Msg, State)};
handle_cast({get, Key, Pid}, State) ->
    Ref = make_ref(),
    dict:store(Ref, Pid, State#state.requests),
    route({get, Key}, Key, Ref, State);

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

%% @doc Init node state
-spec(init_state() -> {ok, #state{}}).

init_state() ->
    {ok, Meta} = hordad_dht_meta:init(),
    {ok, Storage} = hordad_dht_storage:init(),
    {ok, Route} = hordad_dht_route_table:init(),
    {ok, LeafSet} = hordad_dht_leaf_set:init(),
    {ok, NBSet} = hordad_dht_neighborhood_set:init(),

    {ok, #state{meta=Meta,
                storage=Storage,
                route=Route,
                leaf=LeafSet,
                nb=NBSet,
                requests=dict:new()
               }}.

%% @doc Handle incoming service requests
do_service_handler({route, Msg, Id, Ref}, State)
  when is_reference(Ref) ->
    route(Msg, Id, Ref, State).
    
%% @doc Select next node in routing chain
route(Msg, Id, Ref, State) ->
    % First check the leaf set
    case hordad_dht_leaf_set:has_node(State#state.leaf, Id) of
        % The destination node is the current one. Deliver to itself
        self ->
            deliver(Msg, Id, Ref, State);
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
    end,
    
    State.

%% @doc Join existing DHT
-spec(join_dht(IP :: tuple()) -> ok).

join_dht(EntryPoint) ->
    pass.

%% @doc Forward message to another node
forward(Msg, Id, Ref, NodeId) ->
    pass.

%% @doc Deliver a message to this very node
deliver({get, Key}, Id, Ref, State) ->
    {ok, Ref}.
