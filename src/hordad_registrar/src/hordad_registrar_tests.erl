%%% -------------------------------------------------------------------
%%% File    : hordad_registrar_tests
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Tests for hordad_registrar
%%%
%%% Created : 2010-02-08 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_registrar_tests).

-include_lib("eunit/include/eunit.hrl").

f_test() ->
    CB = {test_module, test_function},
    
    ?assertEqual(ok, hordad_registrar:register("test", CB)),
    ?assertEqual(CB, hordad_registrar:get_cb("test")),
    ?assertEqual(undefined, hordad_registrar:get_cb("wtf_test")),
    
    ?assertEqual(ok, hordad_registrar:unregister("test")),
    ?assertEqual(undefined, hordad_registrar:get_cb("test")).

    
