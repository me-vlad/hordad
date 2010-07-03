%%% -------------------------------------------------------------------
%%% File    : hordad_mi_app
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Application callback
%%%
%%% Created : 2010-06-29 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_mi_app).

-behaviour(application).

-include_lib("yaws/include/yaws.hrl").

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
    case start_server() of
        ok ->
            hordad_mi_sup:start_link();
        {error, _}=Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%%--------------------------------------------------------------------
stop(_State) ->
    application:stop(yaws),

    ok.

%% @doc Start yaws
start_server() ->
    [Id, LogDir, Port, DocRoot, Listen, ServerName] =
        hordad_lcf:get_vars(
          [
           {hordad_mi, server_id},
           {hordad_mi, server_logdir},
           {hordad_mi, server_port},
           {hordad_mi, server_docroot},
           {hordad_mi, server_listen_ip},
           {hordad_mi, server_name}
          ]),

    try
        ok = application:load(yaws),
        ok = application:set_env(yaws, embedded, true),
        ok = application:start(yaws),

        DefaultGC = yaws_config:make_default_gconf(false, Id),
        GC = DefaultGC#gconf{logdir = LogDir},
        SC = #sconf{
          port = Port,
          docroot = DocRoot,
          listen = Listen,
          servername = ServerName,
          allowed_scripts = [yaws]
          },

        ok = yaws_api:setconf(GC, [[SC]]),
        
        ok
    catch
        _:E ->
            {error, E}
    end.
