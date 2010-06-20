%%% -------------------------------------------------------------------
%%% File    : hordad_aes_poller
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Hordad AES poller
%%%
%%% Created : 2010-02-11 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_aes_poller).

-behaviour(hordad_service).

%% API
-export([start_link/0]).

%% hordad_service callbacks
-export([service_handler/2]).

-include_lib("hordad_aes_agent/include/hordad_aes_agent.hrl").

-define(SERVICE_TAG, "aes_poller").

start_link() ->
    hordad_service:start_link(?MODULE, ?MODULE, ?SERVICE_TAG).

service_handler({"poll", Ref}, _Socket) ->
    poll_nodes(Ref).

%------------------
% Internal functons
%------------------

%% @doc Poll nodes for physical availability
-spec(poll_nodes(reference()) -> {ok, reference(),
                                  [#agent_report{}]} | error).

poll_nodes(Ref) ->
    Self = self(),
    Poller = spawn(fun() -> poller(Self) end),

    receive
        {Poller, Report} ->
            {ok, Ref, Report};
        Unexpected ->
            hordad_log:error(?MODULE, "Unexpected poller report: ~9999p",
                             [Unexpected]),
            error
    end.

%% @doc Poller engine
poller(Parent) ->
    Nodes = hordad_lcf:get_var({?MODULE, nodes}, []),
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
poll_node(Parent, Ref, Node) ->
    Timeout = hordad_lcf:get_var({?MODULE, poll_timeout}),
    hordad_log:info(?MODULE, "Polling node: ~p", [Node]),

    try
        #agent_report{} = Report = hordad_lib_net:gen_session(
                                     ?MODULE, Node,
                                     "aes_agent", "report", Timeout),

        hordad_log:info(?MODULE, "Node ~p status=~p, lar=~p",
                        [Node, Report#agent_report.status,
                         Report#agent_report.lar]),

        Parent ! {Ref, self(), Node, Report}
    catch
        _:_ ->
            hordad_log:error(?MODULE, "Error polling node ~p: ~9999p",
                             [Node, erlang:get_stacktrace()]),
            Parent ! {Ref, self(), error}
    end.
