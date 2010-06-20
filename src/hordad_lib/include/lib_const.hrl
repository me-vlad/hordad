%%% -------------------------------------------------------------------
%%% File    : lib_const.hrl
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Useful constants
%%%
%%% Created : 2010-01-04 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-ifndef(LIB_CONST_HRL).
-define(LIB_CONST_HRL, true).

% Base system directory
-define(CONST_SYSTEM_BASE, "/usr/hordad").
% Logs dir
-define(CONST_LOG_BASE, "/var/log/hordad").

% Configuration directory
-define(CONST_CONF_SUBDIR, "etc").

% DB directory
-define(CONST_DB_SUBDIR, "db").

% SSL directory
-define(CONST_SSL_SUBDIR, "ssl").

% Local configuration filename
-define(CONST_CONF_LCF, "lcf.conf").

-endif.
