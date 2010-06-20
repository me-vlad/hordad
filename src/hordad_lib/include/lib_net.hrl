%%% -------------------------------------------------------------------
%%% File    : lib_net.hrl
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Useful net macros
%%%
%%% Created : 2010-03-10 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-ifndef(LIB_NET_HRL).
-define(LIB_NET_HRL, true).

-define(is_ipv4, fun({Oct1, Oct2, Oct3, Oct4}) when
        (is_integer(Oct1) andalso Oct1 >= 0 andalso Oct1 =< 255) andalso
        (is_integer(Oct2) andalso Oct2 >= 0 andalso Oct2 =< 255) andalso
        (is_integer(Oct3) andalso Oct3 >= 0 andalso Oct3 =< 255) andalso
        (is_integer(Oct4) andalso Oct4 >= 0 andalso Oct4 =< 255) ->
               true;
        (_) ->
               false
       ).
               
-endif.
