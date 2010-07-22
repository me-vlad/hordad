%%% -------------------------------------------------------------------
%%% File    : hordad_registrar_tests
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Tests for hordad_registrar
%%%
%%% Created : 2010-07-20 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_registrar_tests).

-include_lib("eunit/include/eunit.hrl").

registrar_test() ->
    Tag = "test",
    CB = {?MODULE, test_cb, [some_arg]},

    ?assertEqual(undefined, hordad_registrar:get_cb(Tag)),
    ?assertError(function_clause, 
                 hordad_registrar:register(Tag, wtf)),
    ?assertEqual(ok,
                 hordad_registrar:register(Tag, CB)),
    ?assertEqual(CB, hordad_registrar:get_cb(Tag)),

    ?assertEqual([{Tag, CB}], hordad_registrar:registered()),
    ?assertEqual(ok, hordad_registrar:unregister(Tag)),
    ?assertEqual([], hordad_registrar:registered()),
    ?assertEqual(undefined, hordad_registrar:get_cb(Tag)).

fun_cb_test() ->
    Tag = "fun_test",
    FunCB = fun(_) ->
                    ok
            end,

    ?assertEqual(ok, hordad_registrar:register(Tag, FunCB)),
    ?assertEqual([{Tag, FunCB}], hordad_registrar:registered()),
    ?assertEqual(ok, hordad_registrar:unregister(Tag)),
    ?assertEqual([], hordad_registrar:registered()),
    ?assertEqual(undefined, hordad_registrar:get_cb(Tag)).

override_test() ->
    Tag = "override",
    F1 = fun(_) ->
                 ok
         end,
    F2 = fun(_) ->
                 ok
         end,

    ?assertEqual(undefined, hordad_registrar:get_cb(Tag)),
    ?assertEqual(ok, hordad_registrar:register(Tag, F1)),
    ?assertEqual(ok, hordad_registrar:register(Tag, F2)),
    ?assertEqual(F2, hordad_registrar:get_cb(Tag)).
