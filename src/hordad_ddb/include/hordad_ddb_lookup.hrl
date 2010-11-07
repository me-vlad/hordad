%%% -------------------------------------------------------------------
%%% File    : hordad_ddb_lookup.hrl
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Hordad DDB lookup layer macros
%%%
%%% Created : 2010-11-02 by Max E. Kuznecov <mek@mek.uz.ua>
%%% -------------------------------------------------------------------

-ifndef(HORDAD_DDB_LOOKUP_HRL).
-define(HORDAD_DDB_LOOKUP_HRL, true).

-record(node, {
          id,     % Node ID (integer)
          id_str, % Node ID (string)
          ip      % Node IP
         }).

-endif.
