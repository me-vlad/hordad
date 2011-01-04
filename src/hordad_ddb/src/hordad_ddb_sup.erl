%%% -------------------------------------------------------------------
%%% File    : hordad_ddb_sup
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Application supervisor
%%%
%%% Created : 2010-11-01 by Max E. Kuznecov <mek@mek.uz.ua>
%%% -------------------------------------------------------------------

-module(hordad_ddb_sup).

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
    Lookup = {hordad_ddb_lookup, {hordad_ddb_lookup, start_link,[]},
              permanent, 2000, worker, [hordad_ddb_lookup]},

    {ok, {{one_for_one, 5, 1}, [Lookup]}}.

%%====================================================================
%% Internal functions
%%====================================================================
