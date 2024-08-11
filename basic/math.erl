-module(math).
-export([init/0, add/2, add/3, fac/1]).

add(X, Y) -> X + Y.
add(X, Y, Z) -> X + Y + Z.
fac(0) -> 1;
fac(N) -> N * fac(N-1).

sin(X) -> math:sin(X).
cos(X) -> math:cos(X).
tan(X) -> math:tan(X).
mean(list) when is_list(list) -> lists:sum(list) / length(list).

median(List) when is_list(List) ->
    Sorted = lists:sort(List),
    Len = length(Sorted),
    if
        Len rem 2 == 1 -> lists:nth((Len div 2) + 1, Sorted);
        true -> (lists:nth(Len div 2, Sorted) + lists:nth((Len div 2) + 1, Sorted)) / 2
    end.
init() ->
    List = [1,2,3,4,5,6,7,8,9,10],
    SumList = lists:sum(List),
    io:format("Math Module Loaded~n"),
    io:format("Add numbers 34 with 56: ~n~p",add(34, 56)),
    io:format("Sine: ~p~n", [sin(math:pi() / 2)]),
    io:format("Cosine: ~p~n", [cos(0)]),
    io:format("Tangent: ~p~n", [tan(math:pi() / 4)]),
    io:format("Factorial: ~p~n", [fac(5)]),
    io:format("Sum of the numbers in the list: ~p~n", SumList),
    io:format("Mean: ~p~n", [mean([1, 2, 3, 4, 5])]),
    io:format("Median: ~p~n", [median([1, 2, 3, 4, 5])]),
    ok.