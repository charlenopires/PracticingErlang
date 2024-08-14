-module(tcp_simpleclient).
-export([client/0]).

client() ->
    Host = "localhost", % to make it runnable on one machine
    {ok, Sock} = gen_tcp:connect(Host, 5050, [binary, {packet, 0}]),
    ok = gen_tcp:send(Sock, "Wow, i'm here!"),
    ok = gen_tcp:close(Sock).