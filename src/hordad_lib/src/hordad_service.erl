%%% -------------------------------------------------------------------
%%% File    : hordad_service
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Hordad generic service handler behaviour
%%%
%%% Created : 2010-02-11 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_service).

-export([start_link/3
        ]).

-export([init/4, generic_service_handler/2, generic_service_handler/4]).
-export([system_continue/3, system_terminate/4]).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
     {servce_handler, 2}
    ].

start_link(Name, Module, Tag) ->
    proc_lib:start_link(?MODULE, init, [self(), Name, Module, Tag]).

%% @doc Init service handler
init(Parent, Name, Module, Tag) ->
    register(Name, self()),
    hordad_registrar:register(Tag, {?MODULE, generic_service_handler,
                                    [Module]}),

    proc_lib:init_ack(Parent, {ok, self()}),

    Deb = sys:debug_options([]),

    hordad_log:log(Module, info, "Module started", []),

    loop(Parent, Deb, Module).

%% @doc Event handling loop
loop(Parent, Deb, Module) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent,
                                  ?MODULE, Deb, Module);
        Other ->
            hordad_log:log(?MODULE, warning,
                           "Unknown message received: ~9999p",
                           [Other]),
            loop(Parent, Deb, Module)
        end.

system_continue(Parent, Deb, State) ->
    loop(Parent, Deb, State).

system_terminate(Reason, _Parent, _Deb, _State) ->
    exit(Reason).

generic_service_handler(ClientSocket, Module) ->
    generic_service_handler(ClientSocket, Module, service_handler, []).

%% @doc Generic service handler.
%%      Performs a read-write loop with connected client
-spec(generic_service_handler(Socket :: port(), CBModule :: atom(),
                              CBFun :: function(), Args :: [any()]) -> ok).

generic_service_handler(ClientSocket, Module, CallBack, Args) ->
    try
        hordad_lib_net:rw_loop(Module, ClientSocket, 
                               fun(Data, Socket) ->
                                       apply(Module, CallBack,
                                             [Data, Socket | Args])
                               end)        
    catch
        _:_ ->
            hordad_log:error(Module, "Error in net loop: ~9999p",
                             [erlang:get_stacktrace()])
    end,

    hordad_lib_net:close_socket(?MODULE, ClientSocket),

    ok.
