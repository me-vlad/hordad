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
         make_node/2,
         between_right_inc/3,
         between/3,
         visualize_circle/1
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
-spec(make_node(tuple(), integer()) -> #node{}).

make_node(IP, Port) ->
    StrID = gen_id({IP, Port}),
    NumID = id_str2num(StrID),

    #node{id=NumID,
          id_str=StrID,
          port=Port,
          ip=IP}.

%% @doc Check if provided Id lies in range in circular id space
%% (From, To]
-spec(between_right_inc(integer(), integer(), integer) -> boolean()).

between_right_inc(From, To, Id) ->
    if
        From == To andalso Id == From ->
            true;
        Id > From andalso Id =< To ->
            true;
        % Circular transition
        To < From andalso (Id > From orelse Id =< To) ->
            true;
        true ->
            false
    end.

-spec(between(integer(), integer(), integer) -> boolean()).

%% @doc Check if provided Id lies in range in circular id space
%% (From, To)
between(From, To, Id) ->
    if
        From == To ->
            Id /= From;
        Id > From andalso Id < To ->
            true;
        % Circular transition
        To < From andalso (Id > From orelse Id < To) ->
            true;
        true ->
            false
    end.

%% @doc Generate graphviz file describing current ddb ring
-spec(visualize_circle(string()) -> ok | {error, any()}).

visualize_circle(Path) ->
    IdF = fun(N) ->
                 string:left(N#node.id_str, 8)
         end,

    FmtNode = fun(Id, Node) ->
                      lists:flatten(
                        io_lib:format("~p[shape=doublecircle, "
                                      "label=\"IP=~p:~p\\nId=~s\"]~n",
                                      [Id, Node#node.ip,
                                       Node#node.port, Id]))
              end,

    FmtEdge = fun(From, To) ->
                      lists:flatten(io_lib:format("~p -> ~p~n", [From, To]))
              end,

    [First | _] = Nodes = hordad_ddb_lookup:get_full_circle(),

    Data0 = ["digraph hordad_ddb {\n"],

    {Data1, Last} =
        lists:foldl(
          fun(Node, {Acc, Pred}) ->
                  CId = IdF(Node),
                  C = FmtNode(CId, Node),

                  NewAcc = case Pred of
                               undefined ->
                                   [C | Acc];
                               _ ->
                                   PId = IdF(Pred),

                                   [C, FmtEdge(PId, CId),
                                    FmtEdge(CId, PId) | Acc]
                           end,

                  {NewAcc, Node}
          end, {Data0, undefined}, Nodes),

    L = IdF(Last),
    F = IdF(First),

    Data2 = [FmtEdge(F, L), FmtEdge(L, F) | Data1],

    %% Now connect last node with the first one
    file:write_file(Path, lists:reverse(["};" | Data2])).
