%%% -------------------------------------------------------------------
%%% File    : hordad_ldb
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Hordad local database manager
%%%
%%% Created : 2010-05-07 by Max E. Kuznecov <mek@mek.uz.ua>
%%% -------------------------------------------------------------------

-module(hordad_ldb).

%% API
-export([
         init_db/0,
         start_db/0,
         table_exists/1,
         create_table/2,
         write/1,
         delete/2,
         read/2,
         read/3,
         match/1,
         all_keys/1,
         foldl/3,
         first/1,
         last/1
        ]).

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

%% @doc Get all records with provided key from database or return default value
-spec(read(atom(), any(), any()) -> {ok, [record()]} | {error, any()}).

read(Table, Key, Default) ->
    case read(Table, Key) of
        {ok, []} ->
            {ok, Default};
        X ->
            X
    end.

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
    run_transaction(fun() ->
                            mnesia:foldl(Fun, Acc, Table)
                    end).

%% @doc Return the first entry in table, only meaningful for ordered_set
%%      type tables.
%% @end
-spec(first(atom()) -> {ok, any()} | {error, any()}).

first(Table) ->
    run_transaction(fun() ->
                            mnesia:first(Table)
                    end).

%% @doc Return the last entry in table, only meaningful for ordered_set
%%      type tables.
%% @end
-spec(last(atom()) -> {ok, any()} | {error, any()}).

last(Table) ->
    run_transaction(fun() ->
                            mnesia:last(Table)
                    end).

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
