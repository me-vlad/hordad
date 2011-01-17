%%% -------------------------------------------------------------------
%%% File    : hordad_lib_net.erl
%%% Author  : Max E. Kuznecov <mek@mek.uz.ua>
%%% Description: Hordad network library module
%%%
%%% Created : 2010-01-11 by Max E. Kuznecov <mek@mek.uz.ua>
%%% @copyright 2009-2010 Server Labs
%%% -------------------------------------------------------------------

-module(hordad_lib_net).

-export([
         protocol_version/0,
         header_size/0,
         make_header/1,
         make_message/1,
         extract_header/1,
         extract_message/1,
         read_socket/3,
         read_socket/4,
         close_socket/2,
         read_message/3,
         write_socket/3,
         rw_loop/3,
         connect/2,
         connect/3,
         gen_session/4,
         gen_session/5,
         gen_session/6,
         ip2str/1,
         str2ip/1,
         is_ssl_mode/0,
         get_ssl_opts/0,
         peername/1
        ]).

-define(HEADER_SIZE, 32).
-define(MAX_MESSAGE_SIZE, 65536).

-type(ip_address() :: {integer(), integer(), integer(), integer()}).
-type(address() :: string() | ip_address()).

%% @doc Get current protocol version
-spec(protocol_version() -> binary()).

protocol_version() ->
    <<2>>.

%% @doc Get total header size
-spec(header_size() -> integer()).

header_size() ->
    ?HEADER_SIZE.

%% Make binary header
%% Header format is following:
%% Hordad protocol version - 1 byte
%% Service tag length - 1 byte
%% Service tag - up to 30 bytes
%% Optional padding to 32 bytes in total.
-type(tag() :: list()).
-spec(make_header(tag()) -> binary()).

make_header(Tag) when is_list(Tag) ->
    Version = protocol_version(),
    TagLen = length(Tag),
    Pad = header_size() - (TagLen + 1 + 1),

    true = Pad < header_size(),

    list_to_binary([Version, TagLen, Tag, <<0:Pad/unit:8>>]).

%% @doc Make message, suitable for transfering over network
-spec(make_message(iolist()) -> binary()).

make_message(Message) ->
    Msg = term_to_binary(Message),
    Size = byte_size(Msg),

    true = Size =< ?MAX_MESSAGE_SIZE,

    <<Size:16, Msg/binary>>.

%% @doc Extract service tag from binary header
-spec(extract_header(binary()) -> {Version :: integer(), Tag :: tag()}).

extract_header(<<Ver, Len, RawTag:Len/binary-unit:8, _Pad/binary>>=Header)
  when byte_size(Header) =:= ?HEADER_SIZE ->
    {Ver, binary_to_list(RawTag)}.

%% @doc Unpack raw message
-spec(extract_message(binary()) -> iolist()).
              
extract_message(RawMessage) when is_binary(RawMessage) ->
    binary_to_term(RawMessage).

%% @doc Read data from socket
-spec(read_socket(atom(), port(), integer()) -> {ok, binary()}
                                                    | closed | error).

read_socket(Module, Socket, Size) ->
    read_socket(Module, Socket, Size, infinity).

%% @doc Read data from socket
-spec(read_socket(atom(), port(), integer(), timeout()) ->
             {ok, binary()} | closed | error).

read_socket(Module, Socket, Size, Timeout) ->
    RecvF = case is_ssl_mode() of
                true ->
                    fun ssl:recv/3;
                false ->
                    fun gen_tcp:recv/3
            end,

    case RecvF(Socket, Size, Timeout) of
        {error, closed} ->
            hordad_log:debug(Module, "Client connection closed", []),
            closed;
        {error, Reason} ->
            hordad_log:error(Module, "Client read error: ~p", [Reason]),
            error;
        {ok, Data} ->
            {ok, Data}
    end.

%% @doc Read complete message according to hordad binary protocol
%% Each chunk starts with two-bytes chunk length followed by actual data
read_message(Module, Socket, Timeout) ->
    read_message(Module, Socket, Timeout, <<>>, 0).

read_message(_, _, _, Buffer, 0) when Buffer /= <<>> ->
    {ok, extract_message(Buffer)};
read_message(Module, Socket, Timeout, Buffer, Rest) ->
    Chunk = read_socket(Module, Socket, 0, Timeout),

    case Chunk of
        {ok, <<Size:16/integer, Data/binary>>} when Buffer == <<>> ->
            read_message(Module, Socket, Timeout, Data,
                         Size - byte_size(Data));
        {ok, Data} ->
            read_message(Module, Socket, Timeout,
                       <<Buffer/binary, Data/binary>>, Rest - byte_size(Data));
        E ->
            E
    end.

%% @doc Send data to clien
-spec(write_socket(atom(), port(), binary()) -> ok | error).

write_socket(Module, Socket, Packet) ->
    SendF = case is_ssl_mode() of
                true ->
                    fun ssl:send/2;
                false ->
                    fun gen_tcp:send/2
            end,

    case SendF(Socket, Packet) of
        {error, Reason} ->
            hordad_log:error(Module, "Client send error: ~p", [Reason]),
            error;
        ok ->
            ok
    end.

%% @doc Close socket
-spec(close_socket(atom(), port()) -> ok | error).

close_socket(Module, Socket) ->
    CloseF = case is_ssl_mode() of
                 true ->
                     fun ssl:close/1;
                 false ->
                     fun gen_tcp:close/1
             end,

    case CloseF(Socket) of
        {error, Reason} ->
            hordad_log:error(Module, "Error closing socket: ~p", [Reason]),
            error;
        ok ->
            ok
    end.

%% @doc Read message from socket, pass it to callback function and
%%      send responce to socket back. Function returns when connection closes
%%      or error occurs.
%% @end
-spec(rw_loop(atom(), port(), function()) -> ok | error).

rw_loop(Module, Socket, Cb) ->
    case read_message(Module, Socket, infinity) of
        X when X == error orelse X == closed ->
            ok;
        {ok, Data} ->
            Response = Cb(Data, Socket),

            case write_socket(Module, Socket, make_message(Response)) of
                ok -> 
                    rw_loop(Module, Socket, Cb);
                error ->
                    error
            end
    end.

%% @doc Connect to another node
-spec(connect(address(), integer()) -> {ok, port()} | {error, term()}).

connect(Node, Port) ->
    connect(Node, Port, infinity).

%% @doc Connect to another node
-spec(connect(address(), integer(), integer() | infinity) ->
             {ok, port()} | {error, term()}).

connect(Node, Port, Timeout) ->
    Opts = [binary, {packet, raw}, {active, false}],

    case hordad_lcf:get_var({hordad, ssl}) of
        true ->
            ssl:connect(Node, Port, get_ssl_opts() ++ Opts, Timeout);
        false ->
            gen_tcp:connect(Node, Port, Opts, Timeout)
    end.

%% @doc Generic functions which provide quick way of sending single message
%%      to another hordad node and receiving response.
-spec(gen_session(address(), integer(), string(), any()) ->
             {ok, any()} | {error, any()}).

gen_session(Node, Port, Header, Message) ->
    gen_session(?MODULE, Node, Port, Header, Message, infinity).

-spec(gen_session(address(), integer(), string(), any(), timeout()) ->
             {ok, any()} | {error, any()}).

gen_session(Node, Port, Header, Message, Timeout) ->
    gen_session(?MODULE, Node, Port, Header, Message, Timeout).

-spec(gen_session(atom(), address(), integer(), string(), any(), timeout()) ->
             {ok, any()} | {error, any()}).

gen_session(Module, Node, Port, Header, Message, Timeout) ->
    try
        {ok, S} = connect(Node, Port, Timeout),
        ok = write_socket(Module, S, make_header(Header)),
        ok = write_socket(Module, S, make_message(Message)),
        {ok, R} = read_message(Module, S, Timeout),
        close_socket(Module, S),
        
        {ok, R}
    catch
        _:E ->
            {error, E}
    end.

%% @doc Convert ip from internal to string representation
ip2str(IP) when is_tuple(IP) ->
    inet_parse:ntoa(IP).

%% @doc Convert ip from string to internal representation
str2ip(Str) when is_list(Str) ->
    {ok, IP} = inet_parse:address(Str),
    IP.

%% @doc Check if we're working in SSL mode
-spec(is_ssl_mode() -> boolean()).

is_ssl_mode() ->
    hordad_lcf:get_var({hordad, ssl}).

%% @doc Return list of SSL socket options
-spec(get_ssl_opts() -> [{atom(), any()}]).

get_ssl_opts() ->
    NodeCert =
        hordad_lib:get_file(
          ssl, hordad_lcf:get_var({hordad, ssl_node_certificate})),

    NodeKey =
        hordad_lib:get_file(
          ssl, hordad_lcf:get_var({hordad, ssl_node_key})),
            
    CACert =
        hordad_lib:get_file(
          ssl, hordad_lcf:get_var({hordad, ssl_ca_certificate})),

    [{verify, 2},
     {depth, 1},
     {certfile, NodeCert},
     {keyfile, NodeKey},
     {cacertfile, CACert}
    ].

%% @doc Get IP, port out of client socket
-spec(peername(port()) -> {ok, {ip_address(), integer()}} | {error, atom()}).

peername(Socket) ->
    SSLMode = is_ssl_mode(),

    case SSLMode of
        true ->
            ssl:peername(Socket);
        false ->
            inet:peername(Socket)
    end.
