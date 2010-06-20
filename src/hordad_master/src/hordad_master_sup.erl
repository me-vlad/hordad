%%% -------------------------------------------------------------------
%%% File    : hordad_master_sup
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Toplevel supervisor
%%%
%%% Created : 2010-01-04 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_master_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start/0]).

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

start() ->
    start_link().

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
    LCF = {hordad_lcf, {hordad_lcf_sup, start_link, []},
           permanent, 2000, worker, [hordad_lcf]},

    Log = {hordad_log, {hordad_log_sup, start_link, []},
           permanent, 2000, worker, [hordad_log]},

    Tier2 = {hordad_tier2, {hordad_tier2_sup, start_link, []},
             permanent, infinity, supervisor, []},

    {ok,{{one_for_one, 5, 1}, [LCF, Log, Tier2]}}.

%%====================================================================
%% Internal functions
%%====================================================================
