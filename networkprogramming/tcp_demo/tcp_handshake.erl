-module(tcp_handshake).
-export([start_server/0, start_client/0]).

start_server() ->
    {ok, ListenSocket} = gen_tcp:listen(5000,[binary, {active, false}, {packet, 0}]),
    io:format("Server: Waiting for connection on port 5000 ~n"),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("Server: Connection accepted ~n"),
    {ok, <<"SYN">>} = gen_tcp:recv(Socket, 0),
    io:format("Server: Received SYN ~n"),
    ok = gen_tcp:send(Socket, <<"SYN-ACK">>),
    io:format("Server: Sent SYN-ACK ~n"),
    {ok, <<"ACK">>} = gen_tcp:recv(Socket, 0),
    io:format("Server: Received ACK ~n"),
    io:format("Server: Handshake complete ~n"),
    gen_tcp:close(Socket),
    gen_tcp:close(ListenSocket).

start_client() ->
    {ok, Socket} = gen_tcp:connect("localhost", 5000, [binary, {packet, 0}]),
    io:format("Client: Connected to server ~n"),
    ok = gen_tcp:send(Socket, <<"SYN">>),
    io:format("Client: Sent SYN ~n"),
    {ok, <<"SYN-ACK">>} = gen_tcp:recv(Socket, 0),
    io:format("Client: Received SYN-ACK ~n"),
    ok = gen_tcp:send(Socket, <<"ACK">>),
    io:format("Client: Sent ACK ~n"),
    io:format("Client: Handshake complete ~n"),
    gen_tcp:close(Socket).