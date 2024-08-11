#!/usr/bin/env escript
%% -*- erlang -*-
-module(tcp_echomessage).

-export([start/1]).

start(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}]),
    io:format("Server started on port ~p~n", [Port]),
    accept_loop(ListenSocket).

accept_loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("New connection accepted~n"),
    spawn(fun() -> handle_client(Socket) end),
    accept_loop(ListenSocket).

handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            show_message(Data),
            gen_tcp:send(Socket, Data),
            handle_client(Socket);
        {error, closed} ->
            io:format("Connection closed~n"),
            ok
    end.

show_message(Data) ->
    io:format("Message received: ~p~n", [Data]).

main(_) -> start(8081).
