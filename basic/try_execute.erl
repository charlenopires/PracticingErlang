-module(try_execute).
-export([init/0, main/0]).

init() -> io:format("Hello, world by init!~n").
%erl -noshell -s try_execute init -s init stop

main() -> io:format("Hello, world by main!~n").
%erl -noshell -s try_execute main -s init stop