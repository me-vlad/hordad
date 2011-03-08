%%% -------------------------------------------------------------------
%%% File    : hordad_aes_poller
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Hordad AES poller
%%%
%%% Created : 2010-02-11 by Max E. Kuznecov <mek@mek.uz.ua>
%%% -------------------------------------------------------------------

-module(hordad_aes_poller).

-behaviour(hordad_service).

%% API
-export([start_link/0]).

%% hordad_service callbacks
-export([service_handler/2]).

-include_lib("hordad_lib/include/lib_types.hrl").

-define(SERVER, ?MODULE).
-define(SERVICE_TAG, "aes_poller").

start_link() ->
    hordad_service:start_link(?MODULE, ?MODULE, ?SERVICE_TAG).

%% @doc Handle poll request
%% {poll, [net_node()]} -> [{net_node(), status()}]
service_handler({"poll", Nodes}, _Socket) ->
    poll(Nodes).

%% @doc Poll nodes for physical availability
poll(Nodes) ->
    Self = self(),
    Poller = spawn(fun() -> poller_engine(Self, Nodes) end),

    receive
        {Poller, Report} ->
            Report;
        Unexpected ->
            hordad_log:error(?MODULE, "Unexpected poller report: ~9999p",
                             [Unexpected])
    end.

%% @doc Poller engine
poller_engine(Parent, Nodes) ->
    Me = self(),
    Ref = make_ref(),
    Data = dict:from_list([{X, down} || X <- Nodes]),

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
            {Ref, Pid, Node, Status} ->
                {dict:store(Node, Status, Data), Spawned -- [Pid]};
            {Ref, Pid, error} ->
                {Data, Spawned -- [Pid]}
        end,

    poller_loop(Ref, DataNew, Rest).
   
%% @doc Poll node and send result to a parent
poll_node(Parent, Ref, {IP, Port}=Node) ->
    Timeout = hordad_lcf:get_var({?MODULE, poll_timeout}),

    hordad_log:debug(?MODULE, "Polling node: ~s:~p",
                     [hordad_lib_fmt:fmt_node(Node)]),

    try
        {ok, Status} = hordad_lib_net:gen_session(?MODULE, IP, Port,
                                                  "aes_agent", "status",
                                                  Timeout),
        hordad_log:debug(?MODULE, "Node ~s status=~p",
                         [hordad_lib_fmt:fmt_node(Node), Status]),

        Parent ! {Ref, self(), Node, Status}
    catch
        _:_ ->
            hordad_log:error(?MODULE, "Error polling node ~s: ~p",
                             [hordad_lib_fmt:fmt_node(Node),
                              erlang:get_stacktrace()]),
            Parent ! {Ref, self(), error}
    end.
