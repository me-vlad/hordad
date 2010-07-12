%%% -------------------------------------------------------------------
%%% File    : hordad_gts
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: GeoTargeting system
%%%
%%% Created : 2010-02-10 by Max E. Kuznecov <mek@mek.uz.ua>
%%% -------------------------------------------------------------------
-module(hordad_gts).

-behaviour(gen_server).

%% API
-export([start_link/0,
         process_data/1,
         status/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([get_ldb_tables/0]).

-import(hordad_gts_lib, [
                         get_tp_conf/0,
                         get_tp_by_ip/1,
                         get_fallback/1,
                         get_tp_nodes/2,
                         get_tp_domains/1
                        ]).

-define(SERVER, ?MODULE).
-define(TABLE, gts).

-record(gts, {
          tp,          % TP name
          on_tp,       % Hosting TP name
          on_ip        % Hosting IP addr
          }).

-type(ip() :: {integer(), integer(), integer(), integer()}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Get TP status
-spec(status() -> [{TP :: atom(), OnTP :: atom() | ip(), OnIP :: ip()}]).

status() ->
    gen_server:call(?SERVER, status).

%% @doc Process data collected by aes_ag
process_data(Data) ->
    gen_server:call(?SERVER, {process_data, Data}).

%% @doc hordad_ldb table info callback.
-spec(get_ldb_tables() -> {Name :: atom(), Attrs :: [{atom(), any()}]}).

get_ldb_tables() ->
    {?TABLE, [{attributes, record_info(fields, gts)}]}.

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
handle_call({process_data, Data}, _From, Table) ->
    TPData = get_tp_conf(),

    % Append all ips from tps, where affected IP stated in fallback
    AffectedByFallback =
        fun(Node) ->
                {ok, TP} = get_tp_by_ip(Node),

                lists:foldr(
                  fun({T, TD}, Acc) ->
                          FB = hordad_lib:getv(fallback, TD, []),
                          
                          Affected = lists:member(Node, FB) orelse
                                     lists:member(TP, FB),

                          if Affected ->
                                  [get_tp_nodes(T, any),
                                   expand_fallback(T) | Acc];
                          true ->
                                  Acc
                          end
                  end, [], TPData)
        end,

    % Also append all nodes of tps hosted on affected ips
    Nodes = lists:usort(
              lists:foldr(
                fun(N, Acc) ->
                        AccNew = lists:flatten([AffectedByFallback(N) | Acc]),

                        case hordad_ldb:match(#gts{tp='_', on_tp='_',
                                                   on_ip=N}) of
                            {ok, []} ->
                                AccNew;
                            {ok, List} ->
                                lists:flatten([get_tp_nodes(SrcTP, any) ||
                                                  #gts{tp=SrcTP} <- List])
                                    ++ AccNew
                        end
                end, Data, Data)),

    %% Build appropriate queries
    Queries = 
        lists:usort(
          lists:foldr(fun(Node, Acc) ->
                              case get_tp_by_ip(Node) of
                                  {ok, TP} ->
                                      case handle_data(Node, TP) of
                                          undefined ->
                                              Acc;
                                          QList ->
                                              Acc ++ QList
                                      end;
                                  _ ->
                                      hordad_log:warning(
                                        ?MODULE,
                                        "Unable to get TP for "
                                        "node ~p. Ignoring", [Node]),
                                      Acc
                              end
                      end, [], Nodes)),

    % Execute queries
    case hordad_gts_mysql:run_queries(Queries) of
        ok ->
            ok;
        {error, Reason, Result} ->
            hordad_log:error(?MODULE, "Transaction aborted: ~9999p: ~9999p",
                             [Reason, Result])
    end,

    {reply, ok, Table};
handle_call(status, _From, Table) ->
    Status = hordad_ldb:foldl(
               fun(#gts{tp=TP, on_tp=OnTP, on_ip=OnIP}, Acc) ->
                       [{TP, OnTP, OnIP} | Acc]
               end, [], Table),

    {reply, Status, Table}.

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

%% @doc Expand list of possible TP names in fallback into list of IPs
-spec(expand_fallback(atom()) -> [ip()]).
              
expand_fallback(TP) ->
    case get_fallback(TP) of
        undefined ->
            undefined;
        {ok, FallbackData} ->
            lists:map(fun(Entry) when is_atom(Entry) ->
                              get_tp_nodes(Entry, any);
                         (Entry) ->
                              Entry
                      end, FallbackData)
    end.

%% @doc Handle node state switching
-spec(handle_data(ip(), atom()) -> [Query :: binary()] | undefined).

handle_data(Node, TP) ->
    TPData = hordad_lib:getv(TP, get_tp_conf()),
    Fallback = hordad_lib:getv(fallback, TPData, []),
    DBName = hordad_lib:getv(db, hordad_lcf:get_var({?MODULE, driver_config})),
    Domains = get_tp_domains(TP),

    % First find tps, currently hosted on provided one
    case find_first_available([TP | Fallback]) of
        {IP, IP} ->
            hordad_log:info(?MODULE, "~p on TP ~p changed to IP ~p",
                            [Node, TP, IP]),
            hordad_ldb:write(#gts{tp=TP, on_tp=IP, on_ip=IP}),
            
            [build_query(DBName, TP, IP, Domain) || Domain <- Domains];
        {NewIP, NewTP} ->
            hordad_log:info(?MODULE, "~p on TP ~p changed to IP ~p on TP ~p",
                            [Node, TP, NewIP, NewTP]),
            hordad_ldb:write(#gts{tp=TP, on_tp=NewTP, on_ip=NewIP}),

            [build_query(DBName, TP, NewIP, Domain) || Domain <- Domains];
        undefined ->
            hordad_log:error(?MODULE, 
                             "Unable to find available fallback node "
                             "for ~p (~p)", [Node, TP]),
            []
    end.

%% @doc Find first available node among provided fallback list.
%%      Fallback list can include both atoms as TP names and IPs
-spec(find_first_available([]) -> {ip(), ip() | atom()} | undefined).

find_first_available([]) ->
    undefined;
%% When fallback entry is IP address
find_first_available([IP | Tail]) when is_tuple(IP) ->
    case hordad_aes_ag:status(IP) of
        available ->
            {IP, IP};
        _ ->
            find_first_available(Tail)
    end;
%% When fallback entry is atom (TP name)
find_first_available([TP | Tail]) when is_atom(TP) ->
    case get_tp_nodes(TP, available) of
        % Something found, return first available
        [Node | _] ->
            {Node, TP};
        % No available nodes in this TP, continue to the next one
        [] ->
            find_first_available(Tail)
    end.

%% @doc Build UPDATE query
build_query(DBName, TP, IP, Domain) ->
    list_to_binary(
      lists:flatten(
        io_lib:format("UPDATE ~s.~s SET ~s.rdata='~s' "
                      "WHERE ~p.name = '~s'",
                      [DBName, TP, TP, hordad_lib_net:ip2str(IP),
                       TP, Domain]))).
