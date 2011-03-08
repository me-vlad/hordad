%%% -------------------------------------------------------------------
%%% File    : hordad_aes_agent
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Hordad AES agent
%%%
%%% Created : 2010-02-11 by Max E. Kuznecov <mek@mek.uz.ua>
%%% -------------------------------------------------------------------

-module(hordad_aes_agent).

-behaviour(hordad_service).

%% API
-export([start_link/0]).

%% hordad_service callbacks
-export([service_handler/2]).

-include_lib("hordad_aes_agent/include/hordad_aes_agent.hrl").

-define(SERVICE_TAG, "aes_agent").
-define(STATUS, up).

start_link() ->
    hordad_service:start_link(?MODULE, ?MODULE, ?SERVICE_TAG).

service_handler("status", _Socket) ->
    ?STATUS;

service_handler("lar", _Socket) ->
    calculate_lar().

%------------------
% Internal functons
%------------------

%% @doc Calculate Load Average Ratio
-spec(calculate_lar() -> integer()).

calculate_lar() ->
    case hordad_aes_agent_lib:get_current_connections() of
        {ok, CurCon} ->
            MaxCon = hordad_lcf:get_var({hordad_aes_agent, lar_max_con}),
            trunc(CurCon / MaxCon * 100);
        {error, Reason} ->
            hordad_log:error(?MODULE,
                             "Unable to get current connections count: ~9999p",
                             [Reason]),
            
            % Return 0 as default but it is assumed admin must react on the
            % error in logs.

            0
    end.
