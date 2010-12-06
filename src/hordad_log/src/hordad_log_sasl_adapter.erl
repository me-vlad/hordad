%%% -------------------------------------------------------------------
%%% File    : hordad_log_sasl_adapter.erl
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: SASL integration into hordad_log facility
%%%
%%% Created : 2010-12-05 by Max E. Kuznecov <mek@mek.uz.ua>
%%% -------------------------------------------------------------------

-module(hordad_log_sasl_adapter).

-behaviour(gen_event).

-export([start_link/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
    Res = gen_event:start_link({local, ?SERVER}),
    error_logger:add_report_handler(?MODULE),

    Res.

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%--------------------------------------------------------------------
init([]) ->
    {ok, ok}.

%%--------------------------------------------------------------------
%% Function:  
%% handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% Description:Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event. 
%%--------------------------------------------------------------------

handle_event({error, _, {_Pid, Format, Data}}, State) ->
    hordad_log:error(sasl, Format, Data),
    {ok, State};
handle_event({error_report, _, {_, _, Report}}, State) ->
    hordad_log:error(sasl, "~p", [Report]),
    {ok, State};
handle_event({warning_msg, _, {_Pid, Format, Data}}, State) ->
    hordad_log:warning(sasl, Format, Data),
    {ok, State};
handle_event({warning_report, _, {_, _, Report}}, State) ->
    hordad_log:error(sasl, "~p", [Report]),
    {ok, State};
handle_event({info_msg, _, {_Pid, Format, Data}}, State) ->
    hordad_log:warning(sasl, Format, Data),
    {ok, State};
handle_event({info_report, _, {_, _, Report}}, State) ->
    hordad_log:info(sasl, "~p", [Report]),
    {ok, State};
handle_event(Msg, State) ->
    hordad_log:info(?MODULE, "Unknown event received: ~p", [Msg]),
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_call(Request, State) -> {ok, Reply, State} |
%%                                {swap_handler, Reply, Args1, State1, 
%%                                  Mod2, Args2} |
%%                                {remove_handler, Reply}
%% Description: Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified event 
%% handler to handle the request.
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    {ok, ok, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info, State) -> {ok, State} |
%%                             {swap_handler, Args1, State1, Mod2, Args2} |
%%                              remove_handler
%% Description: This function is called for each installed event handler when
%% an event manager receives any other message than an event or a synchronous
%% request (or a system message).
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description:Whenever an event handler is deleted from an event manager,
%% this function is called. It should be the opposite of Module:init/1 and 
%% do any necessary cleaning up. 
%%--------------------------------------------------------------------
terminate(_Reason, _Handle) ->
    ok.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra) -> {ok, NewState} 
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
