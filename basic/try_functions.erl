#!/usr/bin/env escript
%% -*- erlang -*-
-module(try_functions).

main(_) ->
    io:format("Result of function without arguments: ~p~n", [greet()]),
    io:format("Result of function with one argument: ~p~n", [greet("Charleno")]).

greet() ->
    "Hello!".

greet(Name) ->
    "Hello, " ++ Name ++ "!".

init() -> main().