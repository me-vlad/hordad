%%% -------------------------------------------------------------------
%%% File    : hordad_dht_lib
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Library API for Hordad DHT
%%%
%%% Created : 2010-05-16 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_dht_lib).

-export([
         gen_id/1
        ]).

% httpd_util:hexlist_to_integer("36030949e7d0f5de11a398c5ce520357").

%% @doc Generate hash id for data
-spec(gen_id(Data :: any()) -> integer()).
         
gen_id(Data) ->
    lists:flatten(
      [io_lib:format("~2.16.0b", [N]) ||
          <<N>> <= crypto:sha(term_to_binary(Data))]).
