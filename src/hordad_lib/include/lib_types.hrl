%%% -------------------------------------------------------------------
%%% File    : lib_types.hrl
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Common types declarations
%%%
%%% Created : 2011-01-03 by Max E. Kuznecov <mek@mek.uz.ua>
%%% -------------------------------------------------------------------

-ifndef(LIB_TYPES_HRL).
-define(LIB_TYPES_HRL, true).

-type(ipv4() :: {integer(), integer(), integer(), integer()}).
-type(ipv6() :: undefined).
-type(ip_address() :: ipv4() | ipv6()).
-type(net_port() :: integer()).
-type(net_node() :: {ip_address(), net_port()}).

-endif.
