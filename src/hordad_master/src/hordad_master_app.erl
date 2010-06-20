%%% -------------------------------------------------------------------
%%% File    : hordad_master_app
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Application callback
%%%
%%% Created : 23-12-2009 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_master_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(_Type, _Args) ->
    hordad_master_sup:start_link().

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%%--------------------------------------------------------------------
stop(_State) ->
    ok.
