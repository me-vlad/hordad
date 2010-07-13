%%% -------------------------------------------------------------------
%%% File    : hordad_dht_storage
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Hordad DHT storage
%%%
%%% Created : 2010-02-03 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_dht_storage).

-define(TABLE, dht_storage).

-export([lookup/1,
         lookup/2,
         insert/2
        ]).

-export([get_ldb_tables/0]).

-record(dht_storage, {key, value}).

%% @doc hordad_ldb table info callback.
-spec(get_ldb_tables() -> {Name :: atom(), Attrs :: [{atom(), any()}]}).

get_ldb_tables() ->
    {?TABLE, [{attributes, record_info(fields, dht_storage)}]}.

%% @doc Lookup key value in database
-spec(lookup(any()) -> {ok, any()} | {error, any()}).

lookup(Key) ->
    lookup(Key, undefined).

%% @doc Lookup key value in database
-spec(lookup(any(), any()) -> {ok, any()} | {error, any()}).

lookup(Key, Default) ->
    hordad_ldb:read(?TABLE, Key, Default).

%% @doc Insert new key-value pair into database
-spec(insert(any(), any()) -> ok | {error, any()}).

insert(Key, Value) ->
    hordad_ldb:write(#dht_storage{key=Key, value=Value}).
