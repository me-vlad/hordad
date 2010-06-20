%%% -------------------------------------------------------------------
%%% File    : hordad_storage
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: General persistent storage interface
%%%
%%% Created : 2010-06-20 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_storage).

-export([new/3,
         insert/2
        ]).

%% @doc Create new storage instance
-spec(new(Name :: atom(), string(), any()) ->
             {ok, Name :: atom()} | {error, any()}).

new(Name, FilePath, KeyPosition) ->
    {ok, Name} = dets:open_file(Name, [{keypos, KeyPosition},
                                       {file, FilePath},
                                       {ram_file, true}
                                      ]).

%% @doc Insert new data into storage
-spec(insert(Id :: atom(), [tuple()]) -> ok).

insert(Id, Data) ->
    ok = dets:insert(Id, Data),
    ok = dets:sync(Id),
    ok.
