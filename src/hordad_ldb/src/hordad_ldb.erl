%%% -------------------------------------------------------------------
%%% File    : hordad_ldb
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Hordad local database manager
%%%
%%% Created : 2010-05-07 by Max E. Kuznecov <mek@mek.uz.ua>
%%% -------------------------------------------------------------------

-module(hordad_ldb).

-behaviour(gen_server).

%% API
-export([start_link/0,
         init_db/0,
         start_db/0,
         table_exists/1,
         create_table/2,
         write/1,
         delete/2,
         read/2,
         match/1,
         all_keys/1,
         foldl/3
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Init database
-spec(init_db() -> ok | {error, any()}).

init_db() ->
    try
        case mnesia:system_info(use_dir) of
            true ->
                ok;
            false ->
                mnesia:create_schema([node()])
        end
    catch
        _:E ->
            {error, E}
    end.

%% @doc Start database and create all the tables needed
-spec(start_db() -> ok | {error, any()}).

start_db() ->
    Tables = hordad_lcf:get_var({hordad_ldb, tables}),

    try
        {ok, _} = hordad_lib:ensure_started(mnesia),

        lists:foreach(
          fun({Table, Attrs}) ->
                  case table_exists(Table) of
                      true ->
                          ok;
                      false ->
                          ok = create_table(Table, Attrs)
                  end
          end,
          [X:get_ldb_tables() || X <- Tables]),

        ok
    catch
        _:E ->
            {error, E}
    end.

%% @doc Write record to database
-spec(write(record()) -> ok | {error, any()}).
             
write(Record) ->
    run_transaction(fun() ->
                            mnesia:write(Record)
                    end).

%% @doc Delete record from database
-spec(delete(atom(), any()) -> ok | {error, any()}).
             
delete(Table, Key) ->
    run_transaction(fun() ->
                            mnesia:delete({Table, Key})
                    end).

%% @doc Get all records with provided key from database
-spec(read(atom(), any()) -> {ok, [record()]} | {error, any()}).

read(Table, Key) ->
    run_transaction(fun() ->
                            mnesia:read({Table, Key})
                    end).

%% @doc Check if table already exists
-spec(table_exists(atom()) -> boolean()).

table_exists(Table) ->
    lists:member(Table, mnesia:system_info(tables)).

%% @doc Create new table
create_table(Table, Attrs) ->
    case mnesia:create_table(Table, [{disc_copies, [node()]} | Attrs]) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.

%% @doc Match objects by pattern
-spec(match(record()) -> {ok, [record()]} | {error, any()}).

match(Pattern) ->
    run_transaction(fun() ->
                            mnesia:match_object(Pattern)
                    end).

%% @doc Get list of all keys in table
-spec(all_keys(atom()) -> {ok, [atom()]} | {error, any()}).

all_keys(Table) ->
    run_transaction(fun() ->
                            mnesia:all_keys(Table)
                    end).

%% @doc Perform folding on table entries
foldl(Fun, Acc, Table) ->    
    case mnesia:foldl(Fun, Acc, Table) of
        {abort, Reason} ->
            {error, Reason};
        Res ->
            Res
    end.

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
    {ok, ok}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    application:stop(mnesia),

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

run_transaction(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, ok} ->
            ok;
        {atomic, Result} ->
            {ok, Result};
        {aborted, Reason} ->
            {error, Reason}
    end.
