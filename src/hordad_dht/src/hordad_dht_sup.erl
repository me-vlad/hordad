%%% -------------------------------------------------------------------
%%% File    : hordad_dht_sup
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Application supervisor
%%%
%%% Created : 2010-03-22 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_dht_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    Meta = {hordad_dht_meta, {hordad_dht_meta, start_link,[]},
            permanent, 2000, worker, [hordad_dht_meta]},

    LeafSet = {hordad_dht_leaf_set, {hordad_dht_leaf_set, start_link,[]},
               permanent, 2000, worker, [hordad_dht_leaf_set]},

    NHSet = {hordad_dht_neighborhood_set,
             {hordad_dht_neighborhood_set, start_link,[]},
             permanent, 2000, worker, [hordad_dht_neighborhood_set]},

    Srv = {hordad_dht, {hordad_dht, start_link,[]},
           permanent, 2000, worker, [hordad_dht]},

    {ok, {{one_for_one, 5, 1}, [Meta, LeafSet, NHSet, Srv]}}.

%%====================================================================
%% Internal functions
%%====================================================================
