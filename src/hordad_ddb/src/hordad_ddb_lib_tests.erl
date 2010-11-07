%%% -------------------------------------------------------------------
%%% File    : hordad_ddb_lib_tests
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Tests for hordad_ddb_lib
%%%
%%% Created : 2010-11-06 by Max E. Kuznecov <mek@mek.uz.ua>
%%% -------------------------------------------------------------------

-module(hordad_ddb_lib_tests).

-include_lib("eunit/include/eunit.hrl").

is_node_in_range_test() ->
    Fun = fun hordad_ddb_lib:is_node_in_range_test/3,

    ?assertEqual(true, Fun(70, 110, 90)),
    ?assertEqual(true, Fun(110, 50, 111)),
    ?assertEqual(true, Fun(110, 50, 10)),
    ?assertEqual(true, Fun(110, 50, 50)),

    ?assertEqual(false, Fun(110, 50, 51)),
    ?assertEqual(false, Fun(70, 120, 70)),
    ?assertEqual(false, Fun(90, 10, 50)).
