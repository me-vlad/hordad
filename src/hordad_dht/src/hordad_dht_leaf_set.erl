%%% -------------------------------------------------------------------
%%% File    : hordad_dht_leaf_set
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Hordad DHT leaf set module
%%%
%%% Created : 2010-05-27 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------
-module(hordad_dht_leaf_set).
-behaviour(gen_server).

-include("hordad_dht.hrl").

%% API
-export([start_link/0, has_node/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([get_ldb_tables/0]).

-define(TABLE, dht_leaf_set).
-define(SERVER, ?MODULE).

-type(id() :: integer()).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Check if provided node id falls into our leaf set
-spec(has_node(id()) -> self | false | {has, #dht_leaf_set{}}).

has_node(NodeID) ->
    gen_server:call(?SERVER, {has_node, NodeID}).

%% @doc hordad_ldb table info callback.
-spec(get_ldb_tables() -> {Name :: atom(), Attrs :: [{atom(), any()}]}).

get_ldb_tables() ->
    {?TABLE, [{attributes, record_info(fields, dht_leaf_set)},
              {type, ordered_set}]}.

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
handle_call({has_node, NodeID}, _From, Tab) ->
    {reply, do_has_node(NodeID, Tab), Tab}.

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

%% @doc Workhouse for has_node/1
do_has_node(Id, Tab) ->
    {ok, First} = hordad_ldb:first(Tab),
    {ok, Last} = hordad_ldb:last(Tab),
    SelfId = hordad_lcf:get_var({hordad_dht, node_id}),

    case First of
        % Empty table
        '$end_of_table' ->
            false;
        _ ->
            {ok, [FirstEntry]} = hordad_ldb:read(Tab, First),

            if
                Id < First orelse Id > Last ->
                    false;
                % Get closest node id
                true ->
                    {ok, Next} = hordad_ldb:foldl(
                                   fun(#dht_leaf_set{id=KeyId}=Entry, Acc) when
                                      Id - KeyId < Acc ->
                                           Entry;
                                      (_, Acc) ->
                                           Acc
                                   end, FirstEntry, Tab),

                    if
                        SelfId < Next#dht_leaf_set.id ->
                            self;
                        true ->
                            {has, Next}
                    end
            end
    end.
