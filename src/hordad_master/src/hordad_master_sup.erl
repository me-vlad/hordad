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
    Master = {hordad_master, {hordad_master, start_link, []},
              permanent, infinity, supervisor, []},

    %% Start tier1 apps first
    ok = application:start(hordad_lcf),
    ok = application:start(hordad_log),

    {ok,{{one_for_one, 5, 1}, [Master]}}.

%%====================================================================
%% Internal functions
%%====================================================================
