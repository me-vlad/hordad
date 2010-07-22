%%% -------------------------------------------------------------------
%%% File    : hordad_dht_route_table
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Hordad DHT routing table
%%%
%%% Created : 2010-05-16 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------
-module(hordad_dht_route_table).

-include("hordad_dht.hrl").

%% API
-export([get_node/1,
         get_all_nodes/0
        ]).

-export([get_ldb_tables/0]).

-define(TABLE, dht_route).

%% @doc Get matching node from route table
-spec(get_node(list()) -> undefined | {ok, tuple()} | {error, any()}).

get_node(Key) ->
    Prefix = hordad_dht_lib:shared_prefix(
               Key, hordad_lcf:get_var({hordad_dht, node_id})),
    Col = lists:nth(Prefix + 1, Key),

    case hordad_ldb:read(?TABLE, {Prefix, Col}, undefined) of
        {ok, undefined} ->
            undefined;
        {ok, _IP}=Res ->
            Res;
        E ->
            E
    end.

%% @doc Return list of all nodes stored in routing table
-spec(get_all_nodes() -> [#dht_node{}]).

get_all_nodes() ->
    hordad_ldb:foldl(
      fun(#dht_route{node=Node}, Acc) ->
              [Node | Acc]
      end, [], ?TABLE).

%% @doc hordad_ldb table info callback.
-spec(get_ldb_tables() -> {Name :: atom(), Attrs :: [{atom(), any()}]}).

get_ldb_tables() ->
    {?TABLE, [{attributes, record_info(fields, dht_route)},
              {type, ordered_set}]}.

