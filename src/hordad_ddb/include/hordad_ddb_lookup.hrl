%%% -------------------------------------------------------------------
%%% File    : hordad_ddb_lookup.hrl
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Hordad DDB lookup layer macros
%%%
%%% Created : 2010-11-02 by Max E. Kuznecov <mek@mek.uz.ua>
%%% -------------------------------------------------------------------

-ifndef(HORDAD_DDB_LOOKUP_HRL).
-define(HORDAD_DDB_LOOKUP_HRL, true).

-type(finger_table() :: list()).
-type(node_id() :: integer()).

%% Keyspace length
-define(M, 160).

%% Upper keypspace limit: 2^M
-define(MODULO, 1461501637330902918203684832716283019655932542976).

-record(node, {
          ip,     % Node IP,
          port,   % Node port
          id,     % Node ID (node_id())
          id_str  % Node ID (string())
         }).

-endif.
