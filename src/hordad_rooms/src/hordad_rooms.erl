%%% -------------------------------------------------------------------
%%% File    : hordad_rooms
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Rooms manager
%%%
%%% Created : 2010-01-16 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_rooms).

-behaviour(gen_server).

%% API
-export([start_link/0,
         create/1,
         remove/1,
         force_remove/1,
         join/2,
         leave/2,
         members/1,
         rooms/0,
         send/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(entry, {
          room,    % Room name
          members  % List of member pids
         }).

-record(state, {
          table,
          refs
         }).

-define(DEFAULT_DB_NAME, "hordad_rooms.db").
-define(SERVER, ?MODULE).
-define(DETS_NAME, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Create new room
-spec(create(atom()) -> ok | {error, already_exists} | {error, unknown}).
              
create(Room) ->
    gen_server:call(?SERVER, {create, Room}).

%% @doc Remove room if empty
-spec(remove(atom()) -> ok | {error, not_empty} | {error, Reason :: any()}).

remove(Room) ->
    gen_server:call(?SERVER, {remove, Room}).

%% @doc Unconditionally remove room, if not empty make all members leave in
%%      the first place.
force_remove(Room) ->
    gen_server:call(?SERVER, {force_remove, Room}).

%% @doc Join room
-spec(join(atom(), pid()) -> ok | {error, unknown}).

join(Room, Pid) when is_pid(Pid) ->
    gen_server:call(?SERVER, {join, Room, Pid}).

%% @doc Leave room
-spec(leave(atom(), pid()) -> ok).

leave(Room, Pid) when is_pid(Pid) ->
    gen_server:call(?SERVER, {leave, Room, Pid}).

%% @doc Get room members list
-spec(members(atom()) -> [pid()]).
             
members(Room) ->
    gen_server:call(?SERVER, {members, Room}).

%% @doc Get list of registered rooms
-spec(rooms() -> [atom()]).

rooms() ->
    gen_server:call(?SERVER, rooms).

%% @doc Send message to all room members except sender.
-spec(send(atom(), any()) -> ok).
             
send(Room, Message) ->
    gen_server:call(?SERVER, {send, Room, Message}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    DBPath = hordad_lib:get_file(db,
                                 hordad_lcf:get_var({?MODULE, db},
                                                    ?DEFAULT_DB_NAME)),

    {ok, ?DETS_NAME} = hordad_lib_storage:new(?DETS_NAME, DBPath, #entry.room),

    Refs = setup_monitor(dets:match_object(?DETS_NAME, '$1'), dict:new()),

    hordad_log:log(?MODULE, info, "Module started", []),

    lists:foreach(fun(X) ->
                         create_room(?DETS_NAME, X)
                  end, hordad_lcf:get_var({?MODULE, rooms}, [])),

    {ok, #state{table=?DETS_NAME, refs=Refs}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({create, Room}, _From, #state{table=Table}=State) ->
    Reply = create_room(Table, Room),

    {reply, Reply, State};
handle_call({remove, Room}, _From, #state{table=Table}=State) ->
    Reply = remove_room(Table, Room, false),

    {reply, Reply, State};
handle_call({force_remove, Room}, _From, #state{table=Table}=State) ->
    Reply = remove_room(Table, Room, true),

    {reply, Reply, State};
handle_call({join, Room, Pid}, _From, #state{table=Table, refs=Refs}=State) ->
    {Reply, NewRefs} = case get_room(Table, Room) of
                           [] ->
                               {{error, unknown}, Refs};
                           [#entry{}=E] ->
                               R = ref_member(Room, Pid, Refs),
                               Res = save_room(Table,
                                               E#entry{members=
                                                       [Pid | E#entry.members]
                                                      }),
                               hordad_log:log(?MODULE, info,
                                              "Member ~p joined room ~p",
                                              [Pid, Room]),
                               {Res, R}
            end,

    {reply, Reply, State#state{refs=NewRefs}};
handle_call({leave, Room, Pid}, _From, #state{table=Table, refs=Refs}=State) ->
    {reply, ok, State#state{refs=remove_member(Pid, Table, Room, Refs)}};
handle_call({members, Room}, _From, #state{table=Table}=State) ->
    Reply = case get_room(Table, Room) of
                [] ->
                    [];
                [Entry] ->
                    Entry#entry.members
            end,

    {reply, Reply, State};
handle_call(rooms, _From, #state{table=Table}=State) ->
    {reply, get_rooms(Table), State};
handle_call({send, Room, Msg}, {Pid, _}, #state{table=Table}=State) ->
    spawn(fun() -> send_msg(Table, Room, Pid, Msg) end),

    {reply, ok, State}.
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'DOWN', Ref, process, Pid, Reason}, #state{refs=Refs}=State) ->
    case get_refs(Pid, Refs) of
        [] ->
            hordad_log:log(?MODULE, warning,
                           "Got DOWN message from pid ~p but is unregistered",
                           [Pid]);
        Data ->
            hordad_log:log(?MODULE, info,
                           "Got DOWN message from pid ~p: ~9999p", [Pid, Reason]),

            unsetup_monitor(Data, Ref, State)
    end,

    {noreply, State};
handle_info(Msg, State) ->
    hordad_log:log(?MODULE, warning, "Unknown message received: ~9999p",
                   [Msg]),

    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    hordad_log:info(?MODULE, "Shutting down: ~p~n", [Reason]),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @doc Setup monitor for every pid in entry list
-spec(setup_monitor([#entry{}], dict()) -> dict()).

setup_monitor([], D) ->
    D;
setup_monitor([#entry{room=Room, members=Members} | T], Dict) ->
    setup_monitor(T, lists:foldl(fun(Pid, D) -> ref_member(Room, Pid, D) end,
                                 Dict, Members)).

%% @doc Demonitor room members
-type(ref_entry() :: {{Room :: atom(), pid()}, reference()}).
-spec(unsetup_monitor([ref_entry()], reference(), #state{}) -> none()).

unsetup_monitor(Members, Ref, #state{table=Table, refs=Refs}) ->
    lists:foldr(fun({{Room, Pid}, R}, Acc) when R =:= Ref ->
                          remove_member(Pid, Table, Room, Acc);
                     (_, Acc) ->
                          Acc
                  end, Refs, Members).

%% @doc Setup monitor for new member
-spec(ref_member(atom(), pid(), dict()) -> dict()).

ref_member(Room, Pid, Refs) ->
    dict:store({Room, Pid}, erlang:monitor(process, Pid), Refs).

%% @doc Cancel member monitor
-spec(unref_member(atom(), pid(), dict()) -> dict()).

unref_member(Room, Pid, Refs) ->
    dict:erase({Room, Pid}, Refs).

%% @doc Get list of {{Room, Pid}, Ref} tuples for given Pid
-spec(get_refs(pid(), dict()) -> [ref_entry()]).

get_refs(Pid, Refs) ->
    dict:fold(fun({_, P}=Key, Val, Acc) when P =:= Pid->
                      [{Key, Val} | Acc];
                 (_, _, Acc) ->
                      Acc
              end, [], Refs).

%% @doc Save entry to db
-spec(save_room(atom(), #entry{}) -> ok).

save_room(Table, Room) ->
    ok = hordad_lib_storage:insert(Table, Room),
    ok.

%% @doc Get room entry
-spec(get_room(atom(), atom()) -> [#entry{}] | {error, any()}).

get_room(Table, Room) ->
    dets:lookup(Table, Room).

%% @doc Get rooms
-spec(get_rooms(atom()) -> [atom()]).

get_rooms(Table) ->
    dets:foldr(fun(#entry{room=Room}, Acc) ->
                       [Room | Acc]
               end, [], Table).

%% @doc Remove room member and demonitor it
-spec(remove_member(pid(), atom(), atom(), dict()) -> dict()).

remove_member(Pid, Table, Room, Refs) ->
    case get_room(Table, Room) of
        [] ->
            Refs;
        [#entry{}=E] ->
            R = unref_member(Room, Pid, Refs),
            save_room(Table,
                      E#entry{members=
                              lists:delete(Pid, E#entry.members)}),

            hordad_log:log(?MODULE, info,
                           "Member ~p left room ~p",
                           [Pid, Room]),
            R
    end.

%% @doc Create room.
%% See create/1 spec

create_room(Table, Room) ->
    case dets:lookup(Table, Room) of
        [] ->
            hordad_log:log(?MODULE, info, "Room created: ~p", [Room]),

            save_room(Table, #entry{room=Room, members=[]});
        [_ | _] ->
            {error, already_exists};
        _ ->
            {error, unknown}
    end.

%% @doc Remove room.
%% See remove/1, force_remove/1 spec

remove_room(Table, Room, Force) ->
    case dets:lookup(Table, Room) of
        [] ->
            ok;
        [#entry{}=E] ->
            case E#entry.members of
                [] ->
                    hordad_log:info(?MODULE, "Room removed: ~p", [Room]),
                    dets:delete_object(Table, E);
                _Members ->
                    case Force of
                        false ->
                            {error, not_empty};
                        true  ->
                            dets:delete_object(Table, E)
                    end
            end;
        _ ->
            ok
    end.

send_msg(Table, Room, Pid, Msg) ->
    case get_room(Table, Room) of
        [] ->
            ok;
        [Entry] ->
            Members = lists:delete(Pid, Entry#entry.members),
            lists:foreach(fun(Member) -> Member ! Msg end, Members)
    end.