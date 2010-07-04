%%% -------------------------------------------------------------------
%%% File    : hordad_ldb_app
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Application callback
%%%
%%% Created : 2010-07-04 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_ldb_app).

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
    case start_mnesia() of
        ok ->
            hordad_ldb_sup:start_link();
        E ->
            hordad_log:error(hordad_ldb, "Error starting mnesia: ~999p",
                             [E]),
            E
    end.

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%%--------------------------------------------------------------------
stop(_State) ->
    application:stop(mnesia).

start_mnesia() ->
    application:set_env(mnesia, dir,
                        hordad_lcf:get_var({hordad_ldb, db_dir})),

    ok = hordad_ldb:init_db(),
    ok = hordad_ldb:start_db(),
    ok.

