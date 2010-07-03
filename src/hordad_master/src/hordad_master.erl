%%% -------------------------------------------------------------------
%%% File    : hordad_master.erl
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Application-level supervisor
%%%
%%% Created : 2010-06-30 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_master).

-export([start_link/0]).

%% @doc Start master supervisor
-spec(start_link() -> {ok, pid()}).

start_link() ->
    {ok, spawn_link(fun loop/0)}.

loop() ->
    [RawInterval, Applications] =
        hordad_lcf:get_vars([{hordad_master, interval},
                             {hordad_master, applications}]),

    Interval = RawInterval * 1000,

    lists:foreach(
      fun(App) ->
              case hordad_lib:ensure_started(App) of
                  {ok, started} ->
                      hordad_log:info(?MODULE, "Application ~p started",
                                      [App]);
                  {ok, running} ->
                      ok;
                  {error, E} ->
                      hordad_log:error(?MODULE, "Error starting application "
                                       "~p: ~999p", [App, E])
              end
      end, Applications),

    receive
    after Interval ->
            loop()
    end.