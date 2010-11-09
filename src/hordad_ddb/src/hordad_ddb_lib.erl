%%% -------------------------------------------------------------------
%%% File    : hordad_ddb_lib
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Library API for Hordad DDB
%%%
%%% Created : 2010-05-16 by Max E. Kuznecov <mek@mek.uz.ua>
%%% -------------------------------------------------------------------

-module(hordad_ddb_lib).

-export([
         gen_id/1,
         id_str2num/1,
         make_node/1,
         is_node_in_range/3
        ]).

-include("hordad_ddb_lookup.hrl").

%% @doc Generate hash id for data
-spec(gen_id(Data :: any()) -> string()).
         
gen_id(Data) ->
    lists:flatten(
      [io_lib:format("~2.16.0b", [N]) ||
          <<N>> <= crypto:sha(term_to_binary(Data))]).

%% @doc Convert string id to numerical equivalent
-spec(id_str2num(string()) -> integer()).

id_str2num(Id) when is_list(Id) ->
    erlang:list_to_integer(Id, 16).

%% @doc Generate node id
-spec(make_node(tuple()) -> #node{}).
             
make_node(IP) ->
    StrID = gen_id(IP),
    NumID = id_str2num(StrID),

    #node{id=NumID,
          id_str=StrID,
          ip=IP}.

%% @doc Check if provided node Id lies in range in circular id space
is_node_in_range(From, To, Id) ->
    if
        Id > From andalso Id =< To ->
            true;
        % Circular transition
        To < From andalso (Id > From orelse Id =< To) ->
            true;
        true ->
            false
    end.
