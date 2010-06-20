%%% -------------------------------------------------------------------
%%% File    : hordad_log
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Logger manager
%%%
%%% Created : 2010-01-09 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_log).

-behaviour(gen_event).

%% API
-export([start_link/0,
         log/4,
         log/3,
         error/3,
         warning/3,
         info/3,
         logrotate/0
        ]).

-define(SERVER, ?MODULE).

%% @doc Log a message
-type(log_level() :: error | warning | info).
-spec(log(From :: atom(), log_level(), Format :: string(), Data :: [any()])
      -> ok).

log(From, Level, Format, Data) ->
    % Check if level is in allowed range
    
    case level_allowed(Level) of
        true ->
            gen_event:notify(?SERVER, {From, Level, Format, Data});
        false ->
            ok
    end.

-spec(log(log_level(), string(), [any()]) -> ok).

log(Level, Format, Data) ->
    log(hordad, Level, Format, Data).

%% @doc Log error message. Shorthand for log(From, error, Format, Data).
error(From, Format, Data) ->
    log(From, error, Format, Data).

%% @doc Log warning message. Shorthand for log(From, warning, Format, Data).
warning(From, Format, Data) ->
    log(From, warning, Format, Data).

%% @doc Log info message. Shorthand for log(From, info, Format, Data).
info(From, Format, Data) ->
    log(From, info, Format, Data).

%% @doc Command all hanlders to re-open logs
logrotate() ->
    gen_event:notify(?SERVER, logrotate).

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | {error,Error} 
%% Description: Creates an event manager.
%%--------------------------------------------------------------------
start_link() ->
    Res = gen_event:start_link({local, ?SERVER}),

    %% Add handlers
    Handlers = hordad_lcf:get_var({hordad_log, handlers}, []),

    lists:foreach(fun(Handler) ->
                          gen_event:add_handler(?SERVER, Handler, [])
                  end, Handlers),

    Res.

%% Internal functions

%% @doc Check if required logging level is allowed in conf
-spec(level_allowed(log_level()) -> boolean()).
             
level_allowed(Level) ->
    case hordad_lcf:get_var({hordad_log, levels}, []) of
        all ->
            true;
        List ->
            lists:member(Level, List)
    end.
