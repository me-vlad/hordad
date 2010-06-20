%%% -------------------------------------------------------------------
%%% File    : hordad_aes_agent_lib
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Hordad AES agent library functions
%%%
%%% Created : 2010-03-29 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_aes_agent_lib).

-export([get_current_connections/0]).

%% @doc Read ipvs connections info file and calculate number of currently
%%      active sessions.
%% @end
-spec(get_current_connections() -> {ok, integer()} | {error, any()}).

get_current_connections() ->
    File = hordad_lcf:get_var({hordad_aes_agent, ipvs_conn_file}),

    case file:open(File, read) of
        {error, _R}=Err ->
            Err;
        {ok, Port} ->
            {ok, Re} = re:compile("^.*TCP\\s+.*ESTABLISHED\\s+.*$"),

            Result = read_connections(Port, Re, 0),

            file:close(Port),

            Result
    end.

%% Internal stuff ---------------

%% @doc Read matching connection lines
read_connections(Port, Re, Acc) ->
    case file:read_line(Port) of
        eof ->
            {ok, Acc};
        {error, _}=Err ->
            Err;
        {ok, Line} ->
            NewAcc = case re:run(Line, Re) of
                         {match, _} ->
                             Acc + 1;
                         nomatch ->
                             Acc
                     end,
            read_connections(Port, Re, NewAcc)
    end.
