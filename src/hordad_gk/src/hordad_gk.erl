%%% -------------------------------------------------------------------
%%% File    : hordad_gk
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Hordad GateKeeper
%%%
%%% Created : 2010-02-03 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_gk).

-behaviour(gen_server).

-include_lib("public_key/include/public_key.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([acceptor/1]).

-define(SERVER, ?MODULE).

-record(state, {
          listener,
          acceptor
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
    Port = hordad_lcf:get_var({?MODULE, bind_port}),
    IP = hordad_lcf:get_var({?MODULE, bind_ip}, all),

    RawOpts = [binary,
               {active, false},
               {reuseaddr, true},
               {keepalive, true},
               {packet, raw}
              ] ++ case IP of
                       all -> [];
                       _ -> [{ip, IP}]
                   end,

    {ListenF, Opts} = case hordad_lib_net:is_ssl_mode() of
                          true ->
                              seed_ssl(),

                              {fun ssl:listen/2,
                               RawOpts ++ hordad_lib_net:get_ssl_opts()};
                          false ->
                              {fun gen_tcp:listen/2, RawOpts}
                      end,

    case ListenF(Port, Opts) of
        {ok, Listener} ->
            hordad_log:info(?MODULE, "Server started at ~p:~p", [IP, Port]),

            {ok, #state{listener=Listener, acceptor=make_acceptor(Listener)}};
        {error, Reason} ->
            hordad_log:log(?MODULE, error,
                           "Error opening port ~p on ~p: ~9999p",
                           [Port, IP, Reason]),
            {stop, Reason}
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
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'DOWN', Acceptor, process, _Pid, Info},
            #state{listener=Listener, acceptor=Acceptor}=State) ->

    hordad_log:warning(?MODULE, "Acceptor process down: ~9999p. Respawning",
                       [Info]),
    {noreply, State#state{acceptor=make_acceptor(Listener)}};
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
terminate(Reason, _State) ->
    hordad_log:info(?MODULE, "Shutting down: ~p~n", Reason),

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

%% @doc Spawn acceptor function and setup monitor for it
-spec(make_acceptor(port()) -> reference()).
             
make_acceptor(Listener) ->
    {_Pid, Ref} = spawn_monitor(?MODULE, acceptor, [Listener]),
    Ref.

%% @doc Wait for connection on listen socket and spawn a router on it
-spec(acceptor(port()) -> none()).

acceptor(Listener) ->
    SSLMode = hordad_lib_net:is_ssl_mode(),

    Socket = case SSLMode of
                 true ->
                     case ssl:transport_accept(Listener) of
                         {ok, S} ->
                             case ssl:ssl_accept(S) of
                                 ok ->
                                     S;
                                 {error, closed} ->
                                     hordad_log:info(
                                       ?MODULE,
                                       "Transport socket closed", []),
                                     exit(normal)
                             end;
                         {error, closed} ->
                             hordad_log:info(?MODULE,
                                             "Listen socket closed", []),
                             exit(normal)
                     end;
                 false ->
                     case gen_tcp:accept(Listener) of
                         {ok, S} ->
                             S;
                         {error, closed} ->
                             hordad_log:info(?MODULE,
                                             "Listen socket closed", []),
                             exit(normal)
                     end
             end,

    {ok, {IP, Port}} = hordad_lib_net:peername(Socket),

    hordad_log:info(?MODULE, "Connection accepted from ~p:~p", [IP, Port]),

    case is_valid_peer_cert(Socket, IP) of
        false ->
            ssl:close(Socket),

            hordad_log:warning(?MODULE,
              "Invalid client certificate", []),
            exit(normal);
        true ->
            ok
    end,

    Pid = spawn(fun() -> router(Socket) end),

    ok = case SSLMode of
             true ->
                 ssl:controlling_process(Socket, Pid);
             false ->
                 gen_tcp:controlling_process(Socket, Pid)
         end,

    Pid ! start,

    acceptor(Listener).

%% @doc Read header from socket and try to route the request
-spec(router(port()) -> ok).

router(Socket) ->
    Error = fun(Format, Data) ->
                    hordad_log:error(?MODULE, Format, Data)
            end,

    %% First wait for synchronization
    receive
        start ->
            ok
    after 1000 ->
            Error("Synchronization timeout", []),
            exit(synchronization_failed)
    end,
    
    case hordad_lib_net:read_socket(?MODULE, Socket,
                                    hordad_lib_net:header_size()) of
        {ok, Header} ->
            {_Ver, Tag} = hordad_lib_net:extract_header(Header),
            %% Look for service handler
            case hordad_registrar:get_cb(Tag) of
                undefined ->
                    Error("Undefined handler for tag ~9999p", [Tag]);
                {Module, Fun, Args} ->
                    apply(Module, Fun, [Socket | Args])
            end;
        X when X =:= error; X =:= closed ->
            ok
    end.

%% @doc Ensure client's cert was issued to the same IP the connection
%%      was initiated from.
%% @end.
-spec(is_valid_peer_cert(port(), tuple()) -> boolean()).

is_valid_peer_cert(Socket, RawIP) ->
    try
        {ok, Cert} = ssl:peercert(Socket),
        {ok, OTPCert} = public_key:pkix_decode_cert(Cert, otp),
    
        {rdnSequence, Subject} = (OTPCert#'OTPCertificate'.tbsCertificate)
                                  #'OTPTBSCertificate'.subject,

        {ok, CN} = findCN(Subject),

        IP = hordad_lib_net:ip2str(RawIP),

        if
            IP =/= CN ->
                hordad_log:error(?MODULE,
                                 "Certificate error: "
                                 "CN is ~s but connection "
                                 "requested from ~s", [CN, IP]),
                false;
            true ->
                true
        end
    catch
        _:E ->
            hordad_log:error(?MODULE,
                             "Error validating certificate: ~9999p (~9999p)",
                             [E, erlang:get_stacktrace()]),
            false
    end.

%% @doc Get Common name filed from certificate subject
-spec(findCN([[#'AttributeTypeAndValue'{}]]) -> {ok, list()} | undefined).

findCN(Subj) ->
    lists:foldl(
      fun([#'AttributeTypeAndValue'{type=?'id-at-commonName',
                                    value={printableString, CN}}], _) ->
              {ok, CN};
         (_, Acc) ->
              Acc
      end, undefined, Subj).

%% @doc Seed SSL with random bytes
-spec(seed_ssl() -> ok).

seed_ssl() ->
    {_, A1, A2} = now(),
    random:seed(erlang:phash(node(), 100000),
                erlang:phash(A1, A2),
                A2),

    ssl:seed(integer_to_list(random:uniform(100000))),
    
    ok.
