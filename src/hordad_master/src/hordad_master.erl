%%% -------------------------------------------------------------------
%%% File    : hordad_master.erl
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Application-level supervisor
%%%
%%% Created : 2010-06-30 by Max E. Kuznecov <mek@mek.uz.ua>
%%% -------------------------------------------------------------------

-module(hordad_master).

-export([start_link/0]).

-define(DEFAULT_INTERVAL, 30).

%% @doc Start master supervisor
-spec(start_link() -> {ok, pid()}).

start_link() ->
    {ok, spawn_link(fun loop/0)}.

loop() ->
    RawInterval = hordad_lcf:get_var({hordad_master, interval},
                                     ?DEFAULT_INTERVAL),
    Applications = hordad_lcf:get_var({hordad_master, applications}),
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
