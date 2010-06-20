#!/usr/bin/env python

import os
import time

SUBDIRS = ["ebin", "include", "priv", "src"]

APP_RES_T = """\
{application, %(app)s,
 [{description, "%(desc)s"},
 {vsn, "0.1"},
 {modules, [%(app)s_app, %(app)s_sup]},
 {registered, []},
 {applications, [kernel, stdlib, hordad_lcf, hordad_log]},
 {mod, {%(app)s_app, []}}
 ]}.
"""

APP_CB_T = """\
%%%%%% -------------------------------------------------------------------
%%%%%% File    : %(app)s_app
%%%%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%%%%% Description: Application callback
%%%%%%
%%%%%% Created : %(date)s by Max E. Kuznecov <mek@mek.uz.ua>
%%%%%% @copyright 2009-%(year)s Server Labs
%%%%%% -------------------------------------------------------------------

-module(%(app)s_app).

-behaviour(application).

%%%% Application callbacks
-export([start/2, stop/1]).

%%%%====================================================================
%%%% Application callbacks
%%%%====================================================================
%%%%--------------------------------------------------------------------
%%%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%%%                                     {ok, Pid, State} |
%%%%                                     {error, Reason}
%%%% Description: This function is called whenever an application 
%%%% is started using application:start/1,2, and should start the processes
%%%% of the application. If the application is structured according to the
%%%% OTP design principles as a supervision tree, this means starting the
%%%% top supervisor of the tree.
%%%%--------------------------------------------------------------------
start(_Type, _Args) ->
    %(app)s_sup:start_link().

%%%%--------------------------------------------------------------------
%%%% Function: stop(State) -> void()
%%%% Description: This function is called whenever an application
%%%% has stopped. It is intended to be the opposite of Module:start/2 and
%%%% should do any necessary cleaning up. The return value is ignored. 
%%%%--------------------------------------------------------------------
stop(_State) ->
    ok.
"""

APP_SUP_T = """\
%%%%%% -------------------------------------------------------------------
%%%%%% File    : %(app)s_sup
%%%%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%%%%% Description: Application supervisor
%%%%%%
%%%%%% Created : %(date)s by Max E. Kuznecov <mek@mek.uz.ua>
%%%%%% @copyright 2009-%(year)s Server Labs
%%%%%% -------------------------------------------------------------------

-module(%(app)s_sup).

-behaviour(supervisor).

%%%% API
-export([start_link/0]).

%%%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%%====================================================================
%%%% API functions
%%%%====================================================================
%%%%--------------------------------------------------------------------
%%%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%%%% Description: Starts the supervisor
%%%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%%====================================================================
%%%% Supervisor callbacks
%%%%====================================================================
%%%%--------------------------------------------------------------------
%%%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%%%                     ignore                          |
%%%%                     {error, Reason}
%%%% Description: Whenever a supervisor is started using 
%%%% supervisor:start_link/[2,3], this function is called by the new process 
%%%% to find out about restart strategy, maximum restart frequency and child 
%%%% specifications.
%%%%--------------------------------------------------------------------
init([]) ->
    Child = {%(app)s, {%(app)s, start_link,[]},
          permanent, 2000, worker, [%(app)s]},

    {ok, {{one_for_one, 5, 1}, [Child]}}.

%%%%====================================================================
%%%% Internal functions
%%%%====================================================================
"""

MAKEFILE_T = """\
# Application makefile

include ../Makefile.inc
"""

def error(msg):
    print "***ERROR: %s" % msg

    raise SystemExit()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

app = raw_input("Input application name: ")
desc = raw_input("Input application description: ")
year = time.strftime("%Y")
date = time.strftime("%Y-%m-%d")

if os.path.exists(app):
    error("Application %s already exists!" % app)

# Create dirs
for subdir in SUBDIRS:
    os.makedirs(os.path.join(app, subdir))

# Make app resource file
f = open(os.path.join(app, "ebin", "%s.app" % app), "w")
f.write(APP_RES_T % locals())
f.close()

# Make app callback file
f = open(os.path.join(app, "src", "%s_app.erl" % app), "w")
f.write(APP_CB_T % locals())
f.close()

# Make supervisor file
f = open(os.path.join(app, "src", "%s_sup.erl" % app), "w")
f.write(APP_SUP_T % locals())
f.close()

# Makefile
f = open(os.path.join(app, "Makefile"), "w")
f.write(MAKEFILE_T % locals())
f.close()

print("Done.")
