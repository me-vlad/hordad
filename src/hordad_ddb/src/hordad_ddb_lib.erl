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
                                      "label=\"IP=~s:~p\\nId=~s\"]~n",
                                      [Id, hordad_lib_net:ip2str(Node#node.ip),
                                       Node#node.port, Id]))
              end,

    FmtEdge = fun(From, To) ->
                      lists:flatten(io_lib:format("~p -> ~p~n", [From, To]))
              end,

    Self = hordad_ddb_lookup:get_self(),
    SelfId = IdF(Self),

    FmtFT = fun(Id, FTable) ->
                    {Labels, Conns} = lists:foldr(
                               fun({_, undefined}, {L, C}) ->
                                       {L, C};
                                  ({Idx, Succ}, {L, C}) ->
                                       IP=hordad_lib_net:ip2str(Succ#node.ip),
                                       SId = IdF(Succ),

                                       Lab = lists:flatten(
                                         io_lib:format("~p: ~s:~p (~s)",
                                                       [Idx,
                                                        IP,
                                                        Succ#node.port,
                                                        SId])),

                                       NewC = io_lib:format(
                                                "~s [color=blue, "
                                                "label=\"~p finger\"]",
                                                [FmtEdge(Id, SId), Idx]),

                                       {[Lab | L], [NewC | C]}

                               end, {[], []}, FTable),

                    Entry = Id ++ "-ft",

                    [io_lib:format("~p[shape=box, label=\"~s\"]~n",
                                   [Entry, string:join(Labels, "\\n")]),
                     io_lib:format("~p -> ~p~n", [Id, Entry])
                    ] ++ if
                             Id == SelfId ->
                                 Conns;
                             true ->
                                 []
                         end
            end,

    TruncateFT = fun(FT) ->
                         lists:foldl(fun({_, Prev}, {Acc, Prev}) ->
                                             {Acc, Prev};
                                        ({_, New}=Entry, {Acc, _Prev}) ->
                                             {[Entry | Acc], New}
                                     end, {[], undefined}, FT)
                 end,

    Nodes = hordad_ddb_lookup:get_full_circle(),

    Data = lists:foldl(
             fun({Pred, Node, Succ, RawFT}, Acc) ->
                     CId = IdF(Node),
                     PId = IdF(Pred),
                     SId = IdF(Succ),

                     {FT, _} = TruncateFT(RawFT),

                     [FmtNode(PId, Pred),
                      FmtNode(CId, Node),
                      FmtNode(SId, Succ),

                      FmtEdge(PId, CId),
                      FmtEdge(CId, PId),
                      FmtEdge(CId, SId),
                      FmtEdge(SId, CId),

                      FmtFT(CId, lists:reverse(FT)) | Acc]
             end, [], Nodes),

    %% Now connect last node with the first one
    file:write_file(Path, (["digraph hordad_ddb_ring {\n" |
                            lists:usort(Data)] ++ ["};"])).
