-module(esd).
-export([init/0]).
-export([convert/2]).


convert(M, inch) -> M / 2.54;
convert(N, centimeter) -> N * 2.54.

init() -> 
    io:format(esd:convert(3, inch)),
    io:format(esd:convert(7, centimeter)).