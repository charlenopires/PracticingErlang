-module(fundamental).
-export([add/2, add/3, fac/1]).

%%% Constants
-define(PI, 3.14159).
-define(MAX_SIZE, 100).
-define(SERVER_NAME, "my_server").
-define(TIMEOUT, 5000).

circle_area(Radius) -> ?PI * Radius * Radius.
max_size() -> ?MAX_SIZE.
server_name() -> ?SERVER_NAME.
server_timeout() -> ?TIMEOUT.

%%% Data types and variables
% Atoms
atom_data_types() ->    
    Hello = hello,
    Status = ok,
    Exit = exit,
    Kill = kill,
    {Hello, Status, Exit, Kill}.

%% Numbers
% Integers
integers_data_types() -> 
    Zero = 0,
    One = 1,
    TwentyFive = 25,
    OneThousands = 1000,
    NegativeOne = -1,
    {Zero, One, TwentyFive, OneThousands, NegativeOne}.

% Floats
floats_data_types() -> 
    Pi = 3.14159,
    Height = 5.6,
    Width = 10.3,
    Area = Height * Width,
    {Pi, Height, Width, Area}.

% Strings
strings_data_types() -> 
    Name = "John Doe",
    Address = "123 Main St.",
    Uppercase = string:to_upper(Name),
    Length = string:length(Address),
    {Name, Address, Uppercase, Length}.

% Lists
lists_data_types() -> 
    Days = [monday, tuesday, wednesday, thursday, friday, saturday, sunday],
    Numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    Mixed = [1, 2, 3, "four", "five", 6.0, 7.0],
    First = hd(Numbers),
    Rest = tl(Numbers),
    Sum = lists:sum(Numbers),
    Max = lists:max(Numbers),
    Min = lists:min(Numbers),
    Reverse = lists:reverse(Numbers),
    Append = [1, 2] ++ [3],
    Concatenate = [1, 2, 3] ++ [4, 5, 6],
    {Days, Numbers, Mixed, First, Rest, Sum, Max, Min, Reverse, Append, Concatenate}.

% Tuples
tuples_data_types() -> 
    Point = {3, 4},
    Rectangle = {rectangle, 5, 10},
    Circle = {circle, 5},
    {Point, Rectangle, Circle}.

% Maps
maps_data_types() ->
    Map = #{name => "John Doe", address => "123 Main St."},
    Name = maps:get(name, Map),
    Address = maps:get(address, Map),
    Keys = maps:keys(Map),
    Values = maps:values(Map),
    Size = maps:size(Map),
    {Map, Name, Address, Keys, Values, Size}.

%%% Functions
% Anonymous functions
functions_data_types() ->
    Double = fun(X) -> X * 2 end,
    Area = fun({rectangle, Height, Width}) -> Height * Width;
            ({circle, Radius}) -> ?PI * Radius * Radius
        end,
    DoubleResult = Double(5),
    AreaResult = Area({rectangle, 5, 10}),
    {DoubleResult, AreaResult}.

% Functions with multiple clauses
add(X, Y) -> X + Y.
add(X, Y, Z) -> X + Y + Z.
fac(1) -> 1;
fac(N) -> N * fac(N - 1).
main() -> ok.