%%% -------------------------------------------------------------------
%%% File    : hordad_log_sup
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Application supervisor
%%%
%%% Created : 2010-01-09 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_log_sup).

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
    Logger = {hordad_log, {hordad_log, start_link,[]},
              permanent, 2000, worker, [hordad_log]},

    SASLAdapter = {hordad_log_sasl_adapter,
                   {hordad_log_sasl_adapter, start_link,[]},
                   permanent, 2000, worker, [hordad_log_sasl_adapter]},

    {ok,{{one_for_one, 5, 1}, [Logger, SASLAdapter]}}.

%%====================================================================
%% Internal functions
%%====================================================================
