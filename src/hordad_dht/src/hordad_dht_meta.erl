%%% -------------------------------------------------------------------
%%% File    : hordad_dht_meta
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Hordad DHT metadata database
%%%
%%% Created : 2010-05-16
%%% @copyright 2009-2010 Max E. Kuznecov <mek@mek.uz.ua>
%%% -------------------------------------------------------------------
-module(hordad_dht_meta).

%% API
-export([lookup/1,
         lookup/2,
         insert/1,
         insert/2,
         delete/1
        ]).

-export([get_ldb_tables/0]).

-define(TABLE, dht_meta).

-record(dht_meta, {
          key,
          value
          }).

%% @doc Lookup key value
-spec(lookup(any()) -> any()).

lookup(Key) ->
    do_lookup(Key, default).

%% @doc Lookup key value providing default one
-spec(lookup(any(), any()) -> any()).

lookup(Key, Default) ->
    do_lookup(Key, Default).

%% @doc Insert key-value pair
-spec(insert(any(), any()) -> ok).

insert(Key, Value) ->
    do_insert([{Key, Value}]).

%% @doc Insert list of key-value pairs
-spec(insert([{any(), any()}]) -> ok).

insert(Data) ->
    do_insert(Data).

%% @doc Delete entry
-spec(delete(any()) -> ok).

delete(Key) ->
    ok = hordad_ldb:delete(?TABLE, Key),
    ok.

%% @doc hordad_ldb table info callback.
-spec(get_ldb_tables() -> {Name :: atom(), Attrs :: [{atom(), any()}]}).

get_ldb_tables() ->
    {?TABLE, [{attributes, record_info(fields, dht_meta)}]}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @doc Workhouse for lookup/2
do_lookup(Key, Default) ->
    case hordad_ldb:read(?TABLE, Key) of
        {ok, []} ->
            Default;
        {ok, List} when is_list(List) ->
            [V || #dht_meta{value=V} <- List];
        {error, Reason} ->
            hordad_log:warning(?MODULE,
                               "Error in DB lookup: ~9999p", [Reason]),
            Default
    end.

%% @doc Workhouse for insert/2
do_insert(Data) ->
    lists:foreach(fun({K, V}) ->
                          hordad_ldb:write(#dht_meta{key=K,
                                                     value=V})
                  end, Data),
    ok.
