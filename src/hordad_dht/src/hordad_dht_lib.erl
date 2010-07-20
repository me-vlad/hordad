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
         gen_id/1,
         shared_prefix/2
        ]).

% httpd_util:hexlist_to_integer("36030949e7d0f5de11a398c5ce520357").

%% @doc Generate hash id for data
-spec(gen_id(Data :: any()) -> string()).
         
gen_id(Data) ->
    lists:flatten(
      [io_lib:format("~2.16.0b", [N]) ||
          <<N>> <= crypto:sha(term_to_binary(Data))]).

%% @doc Get length of shared prefix of two keys
shared_prefix(A, B) ->
    shared_prefix(A, B, 0).

shared_prefix([X | T1], [X | T2], Acc) ->
    shared_prefix(T1, T2, Acc + 1);
shared_prefix(_, _, Acc) ->
    Acc.
