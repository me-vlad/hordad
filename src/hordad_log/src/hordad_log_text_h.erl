%%% -------------------------------------------------------------------
%%% File    : hordad_log_text_h
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Plain text logger handler
%%%
%%% Created : 2010-01-10 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_log_text_h).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_LOG_FILE, "hordad_text.log").

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%--------------------------------------------------------------------
init([]) ->
    {ok, open_file()}.

%%--------------------------------------------------------------------
%% Function:  
%% handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% Description:Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event. 
%%--------------------------------------------------------------------
handle_event({From, Level, Format, Data}, Handle) ->
    Msg = lists:flatten(io_lib:format(Format, Data)),

    FormattedMsg = lists:flatten(
                     io_lib:format("[~s] ~p ~p: ~s~n",
                                   [hordad_lib:datetime(local), From, Level,
                                    Msg])),

    case Handle of
        null ->
            Format = case Level of 
                         error -> error_msg;
                         warning -> warning_msg;
                         info -> info_msg;
                         _ -> error_msg
                     end,
            
            error_logger:Format(FormattedMsg);
        H ->
            io:format(H, FormattedMsg, [])
    end,

    {ok, Handle};

handle_event(logrotate, Handle) ->
    NewHandler = case Handle of
                     null ->
                         open_file();
                     Old ->
                         file:close(Old),
                         open_file()
                 end,

    {ok, NewHandler}.

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
    Reply = ok,
    {ok, Reply, State}.

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
terminate(_Reason, null) ->
    ok;
terminate(_Reason, Handle) ->
    file:close(Handle),
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

%% @doc Open log file
-spec(open_file() -> null | port()).

open_file() ->
    File = hordad_lib:get_file(log,
                               hordad_lcf:get_var(
                                 {hordad_log, text_log_file},
                                ?DEFAULT_LOG_FILE)),

    case file:open(File, [append]) of
        {ok, IO} ->
            IO;
        {error, Reason} ->
            error_logger:error_msg("Unable to open log file ~p: ~p",
                                   [File, Reason]),
            null
    end.
