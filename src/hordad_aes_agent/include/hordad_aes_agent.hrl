%%% -------------------------------------------------------------------
%%% File    : hordad_aes_agent.hrl
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Hordad AES agent include file
%%%
%%% Created : 2010-03-29 by Max E. Kuznecov <mek@mek.uz.ua>
%%% -------------------------------------------------------------------

-ifndef(HORDAD_AES_AGENT_HRL).
-define(HORDAD_AES_AGENT_HRL, true).

%% Report record definition
-record(agent_report, {
          status=down,   % Node status
          lar=0          % Node LAR
          }
       ).

-endif.
