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

gen_id_test() ->
    Term = {1,2,3,4, ["AAA", "BBB"]},
    
    ?assertEqual(true,
                 hordad_ddb_lib:gen_id(Term) == hordad_ddb_lib:gen_id(Term)),

    "0ff05e2a6b13dd2f3aba4f1e0f215a9534e409fc" =
        hordad_ddb_lib:gen_id({127,0,0,1}).


