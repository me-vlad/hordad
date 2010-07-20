%%% -------------------------------------------------------------------
%%% File    : hordad_dht_leaf_set
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Hordad DHT leaf set module
%%%
%%% Created : 2010-05-27 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------
-module(hordad_dht_leaf_set).

-include("hordad_dht.hrl").

%% API
-export([has_node/1]).

-export([get_ldb_tables/0]).

-define(TABLE, dht_node).

-type(id() :: integer()).

%% @doc Check if provided node id falls into our leaf set
-spec(has_node(id()) -> self | false | {has, #dht_node{}}).

has_node(NodeID) ->
    {ok, First} = hordad_ldb:first(?TABLE),
    {ok, Last} = hordad_ldb:last(?TABLE),
    SelfId = hordad_lcf:get_var({hordad_dht, node_id}),

    case First of
        % Empty table
        '$end_of_table' ->
            false;
        _ ->
            {ok, [FirstEntry]} = hordad_ldb:read(?TABLE, First),

            if
                NodeID < First orelse NodeID > Last ->
                    false;
                % Get closest node id
                true ->
                    {ok, Next} = hordad_ldb:foldl(
                                   fun(#dht_node{id=KeyId}=Entry, Acc) when
                                      NodeID - KeyId < Acc ->
                                           Entry;
                                      (_, Acc) ->
                                           Acc
                                   end, FirstEntry, ?TABLE),

                    if
                        SelfId < Next#dht_node.id ->
                            self;
                        true ->
                            {has, Next}
                    end
            end
    end.

%% @doc hordad_ldb table info callback.
-spec(get_ldb_tables() -> {Name :: atom(), Attrs :: [{atom(), any()}]}).

get_ldb_tables() ->
    {?TABLE, [{attributes, record_info(fields, dht_node)},
              {type, ordered_set}]}.
