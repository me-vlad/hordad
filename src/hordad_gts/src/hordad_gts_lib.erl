%%% -------------------------------------------------------------------
%%% File    : hordad_gts_lib
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: GeoTargeting system library
%%%
%%% Created : 2010-03-30 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_gts_lib).

-export([get_tp_conf/0,
         get_tp_by_ip/1,
         get_max_lar/1,
         get_fallback/1,
         get_tp_nodes/2,
         get_tp_domains/1
        ]).

-type(fallback_entity() :: TP :: atom() | tuple()).

%% @doc Get configured TP data
-spec(get_tp_conf() -> [{any(), any()}]).

get_tp_conf() ->
    hordad_lcf:get_var({hordad_gts, tp}).

%% @doc Get TP where provided IP belongs to
-spec(get_tp_by_ip(tuple()) -> {ok, atom()} | undefined).

get_tp_by_ip(IP) ->
    lists:foldr(fun({TPName, TPData}, State) ->
                        IPS = hordad_lib:getv(ips, TPData),

                        case lists:member(IP, IPS) of
                            true ->
                                {ok, TPName};
                            _ ->
                                State
                        end
                end, undefined, get_tp_conf()).

%% @doc Get list of fallback nodes/tps
-spec(get_fallback(atom()) -> {ok, [fallback_entity()]}).

get_fallback(TP) ->
    TPConf = get_tp_conf(),

    case hordad_lib:getv(TP, TPConf, undefined) of
        undefined ->
            undefined;
        TPData ->
            {ok, hordad_lib:getv(fallback, TPData, [])}
    end.

%% @doc Return list of IP addresses belonging to the TP in status Status
-spec(get_tp_nodes(atom(), down | available | any) -> [tuple()]).

get_tp_nodes(TP, Status) ->
    TPData = hordad_lib:getv(TP, get_tp_conf()),
    IPS = hordad_lib:getv(ips, TPData, []),

    lists:filter(fun(IP) ->
                         S = hordad_aes_ag:status(IP),
                         
                         if
                             Status =:= any orelse S == Status ->
                                 true;
                             true ->
                                 false
                         end
                 end, IPS).

%% @doc Return list of TP domains
-spec(get_tp_domains(atom()) -> [list()]).

get_tp_domains(TP) ->
    TPData = hordad_lib:getv(TP, get_tp_conf()),
    hordad_lib:getv(domains, TPData, []).

%% @doc Return LAR for provided node from configuration
%%      If no defined LAR value found, return default {hordad_gts, default_lar}
-spec(get_max_lar(tuple()) -> integer()).

get_max_lar(IP) ->
    DefaultLar = hordad_lcf:get_var({hordad_gts, default_lar}),

    case get_tp_by_ip(IP) of
        undefined ->
            DefaultLar;
        {ok, TP} ->
            TPData = hordad_lib:getv(TP, get_tp_conf()),
            LARData = hordad_lib:getv(lar, TPData, []),

            lists:foldl(fun({Node, LAR}, _) when Node =:= IP ->
                                LAR;
                           (_, Acc) ->
                                Acc
                        end, DefaultLar, LARData)
    end.
