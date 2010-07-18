%%% -------------------------------------------------------------------
%%% File    : hordad_tests_app
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Application callback
%%%
%%% Created : 2010-07-19 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_tests_app).

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

    ok = case init:get_argument(tests) of
             error ->
                 ok;
             {ok, Tests} ->
                 try
                    run_tests(lists:foldl(
                                fun(X, Acc) ->
                                        X ++ Acc
                                end, [], Tests))
                 catch
                     _:_ ->
                         ok
                 end
         end,

    halt().

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored.
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

run_tests([]) ->
    ok;
run_tests([Test | Tail]) ->
    io:format("~n>>>>>>>>>>>>>>>>>>> Running ~s:test()~n", [Test]),
    apply(list_to_atom(Test), test, []),

    run_tests(Tail).
