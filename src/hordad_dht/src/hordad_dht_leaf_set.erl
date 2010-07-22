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
-export([has_node/1,
        get_all_nodes/0
        ]).

-export([get_ldb_tables/0]).

-define(TABLE, dht_node).

-type(id() :: integer()).

%% @doc Check if provided node id falls into our leaf set
-spec(has_node(id()) -> self | false | {has, #dht_node{}}).

has_node(NodeIdStr) ->
    {ok, First} = hordad_ldb:first(?TABLE),
    {ok, Last} = hordad_ldb:last(?TABLE),
    FirstNum = hordad_dht_lib:id_str2num(First),
    LastNum = hordad_dht_lib:id_str2num(Last),

    Self = hordad_lcf:get_var({hordad_dht, node}),
    NodeId = hordad_dht_lib:id_str2num(NodeIdStr),

    case First of
        % Empty table
        '$end_of_table' ->
            false;
        _ ->
            {ok, [FirstEntry]} = hordad_ldb:read(?TABLE, First),

            if
                NodeId < FirstNum orelse NodeId > LastNum ->
                    false;
                % Get closest node id
                true ->
                    {ok, Next} =
                        hordad_ldb:foldl(
                          fun(#dht_node{id_num=Key}=Entry, Acc) when
                             NodeId - Key < Acc ->
                                  Entry;
                             (_, Acc)->
                                  Acc
                          end, FirstEntry, ?TABLE),

                    if
                        Self#dht_node.id_num < Next#dht_node.id_num ->
                            self;
                        true ->
                            {has, Next}
                    end
            end
    end.

%% @doc Return list of all nodes stored in leaf set
-spec(get_all_nodes() -> [#dht_node{}]).

get_all_nodes() ->
    case hordad_ldb:match(#dht_node{id='_', id_num='_', ip='_'}) of
        {ok, Nodes} ->
            Nodes;
        _ ->
            []
    end.

%% @doc hordad_ldb table info callback.
-spec(get_ldb_tables() -> {Name :: atom(), Attrs :: [{atom(), any()}]}).

get_ldb_tables() ->
    {?TABLE, [{attributes, record_info(fields, dht_node)},
              {type, ordered_set}]}.
