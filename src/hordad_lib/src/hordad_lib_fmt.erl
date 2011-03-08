%%% -------------------------------------------------------------------
%%% File    : hordad_lib_fmt.erl
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Formatting library
%%%
%%% Created : 2011-03-07 by Max E. Kuznecov <mek@mek.uz.ua>
%%% -------------------------------------------------------------------

-module(hordad_lib_fmt).

-export([fmt_node/1
        ]).

-include_lib("hordad_lib/include/lib_types.hrl").

%% @doc Format net_node() object
-spec(fmt_node(net_node()) -> string()).
              
fmt_node({IP, Port}) ->
    lists:flatten(io_lib:format("~s:~p", [hordad_lib_net:ip2str(IP), Port])).
