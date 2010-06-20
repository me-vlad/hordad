%%% -------------------------------------------------------------------
%%% File    : hordad_rooms_tests
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Tests for hordad_rooms
%%%
%%% Created : 2010-02-08 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_rooms_tests).

-include_lib("eunit/include/eunit.hrl").

room_create_test() ->
    ?assertNot(lists:member(test, hordad_rooms:rooms())),
    ?assertEqual(hordad_rooms:create(test), ok),
    ?assert(lists:member(test, hordad_rooms:rooms())),
    ?assertEqual(hordad_rooms:create(test), {error, already_exists}),
    ?assertEqual(hordad_rooms:remove(test), ok),
    ?assertNot(lists:member(test, hordad_rooms:rooms())),

    ?assertEqual(hordad_rooms:create(test), ok),
    ?assertEqual(ok, hordad_rooms:join(test, self())),
    ?assertEqual(ok, hordad_rooms:force_remove(test)).

f_test_() ->
    {spawn,
     {setup,
      fun() ->
              hordad_rooms:create(test)
      end,
      fun(_) ->
              hordad_rooms:force_remove(test)
      end,
      [
       %% Join/leave
       fun() ->
               Pid = self(),

               ?assertEqual(ok, hordad_rooms:join(test, Pid)),
               ?assertEqual({error, not_empty}, hordad_rooms:remove(test)),
               ?assertEqual(ok, hordad_rooms:leave(test, Pid))
       end,

       %% members
       fun() ->
               Pid = self(),
               ?assertEqual(ok, hordad_rooms:join(test, Pid)),

               ?assertEqual(true, lists:member(Pid,
                                               hordad_rooms:members(test)))
       end,

       %% send
       fun() ->
               Pid = self(),
               ?assertEqual(ok, hordad_rooms:join(test, Pid)),

               ?assertEqual(ok, hordad_rooms:send(test, test_message)),

               F = fun() ->
                           receive
                               test_message ->
                                   test_message;
                               _ ->
                                   invalid
                           after
                               1000 -> timeout
                           end
                   end,

               ?assertEqual(test_message, F())
       end
      ]}}.
