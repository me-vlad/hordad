%%% -------------------------------------------------------------------
%%% File    : hordad_lcf_tests
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Tests for hordad_lcf
%%%
%%% Created : 2010-07-20 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_lcf_tests).

-include_lib("eunit/include/eunit.hrl").

get_var_test() ->
    ?assertEqual("ABCDEF", hordad_lcf:get_var({hordad_lcf, xyz1})),
    ?assertEqual({[]}, hordad_lcf:get_var({hordad_lcf, xyz2})),
    ?assertEqual(undefined, hordad_lcf:get_var({hordad_lcf, nonexistent})),

    ?assertEqual(custom, hordad_lcf:get_var({hordad_lcf, nonexistent},
                                            custom)),

    ?assertEqual(["ABCDEF", {[]}],
                 hordad_lcf:get_vars([{hordad_lcf, xyz1},
                                      {hordad_lcf, xyz2}])).

get_all_test() ->
    ?assertEqual(6, length(hordad_lcf:get_all())).

set_var_test() ->
    ?assertEqual(undefined, hordad_lcf:get_var({hordad_lcf, custom})),
    ?assertEqual(ok, hordad_lcf:set_var({hordad_lcf, custom}, custom_value)),
    ?assertEqual(custom_value, hordad_lcf:get_var({hordad_lcf, custom})).
   
reload_test() ->
    % 5 base plus custom set
    ?assertEqual(7, length(hordad_lcf:get_all())),
    ?assertEqual(ok, hordad_lcf:reload()),
    ?assertEqual(6, length(hordad_lcf:get_all())).
