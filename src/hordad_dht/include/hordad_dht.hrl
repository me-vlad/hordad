%%% -------------------------------------------------------------------
%%% File    : hordad_dht.hrl
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Hordad DHT macros
%%%
%%% Created : 2010-06-24 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-ifndef(HORDAD_DHT_HRL).
-define(HORDAD_DHT_HRL, true).

-record(dht_node, {
          id,     % Node ID (string)
          id_num, % Node ID (integer)
          ip      % Node IP
         }).

-record(dht_route, {
          key,  % {prefix, n+1}
          node  % dht_node{}
         }).

-endif.
