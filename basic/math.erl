-module(math).
-export([init/0, add/2, add/3, fac/1]).

add(X, Y) -> X + Y.
add(X, Y, Z) -> X + Y + Z.
fac(0) -> 1;
fac(N) -> N * fac(N-1).

init() ->
    io:format("math module loaded~n"),
    io:format("~n~p",add(34, 56)),
    ok.