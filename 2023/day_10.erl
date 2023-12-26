-module(day_10).
-export([start_1/1, start_2/1]).
-export([grid_from_binary/1, calculate_enclosure/1]).

-define(EPSILON, 0.5).

-record(grid, {
    binary :: binary(),
    width :: integer(),
    height :: integer()
}).

grid_from_binary(Binary) ->
    {Width, 1} = binary:match(Binary, <<"\n">>),
    TotalSize = binary:referenced_byte_size(Binary),
    Height = (TotalSize + 1) div (Width + 1),
    #grid{
        binary = Binary,
        width = Width,
        height = Height
    }.

at(Grid = #grid{width = Width, height = Height}, {X, Y})
        when 0 =< X andalso X < Width andalso 0 =< Y andalso Y =< Height ->
    %% NOTE: "+ 1" because of the newline character.
    binary:at(Grid#grid.binary, X + Y * (Grid#grid.width + 1)).

find(_Grid = #grid{binary = Binary, width = Width}, Entry) ->
    {Spot, 1} = binary:match(Binary, <<Entry>>),
    {Spot rem (Width + 1), Spot div (Width + 1)}.

start_1([Filename]) ->
    {ok, Binary} = file:read_file(Filename),
    Grid = grid_from_binary(Binary),
    Result = chase(Grid),
    io:format("Result: ~w~n", [Result]),
    halt(0).

start_2([Filename]) ->
    {ok, Binary} = file:read_file(Filename),
    Grid = grid_from_binary(Binary),
    Result = calculate_enclosure(Grid),
    io:format("~nResult: ~w~n", [map_size(#{K => [] || K := V <- Result, abs(V) > ?EPSILON})]),
    halt(0).

chase(Grid) ->
    InitialPos = find(Grid, $S),
    [FirstPos, SecondPos] = possibilities_from_S(Grid, InitialPos),
    chase(Grid, FirstPos, InitialPos, SecondPos, InitialPos, 1).

chase(_Grid, FirstPos, _FirstPrev, _SecondPos = FirstPos, _SecondPrev, Count) ->
    Count;
chase(_Grid, FirstPos, FirstPrev, _SecondPos = FirstPrev, _SecondPrev = FirstPos, Count) ->
    Count;
chase(Grid, FirstPos, FirstPrev, SecondPos, SecondPrev, Count) ->
    [FirstNext] = [P || P <- possibilities(Grid, FirstPos), P /= FirstPrev],
    [SecondNext] = [P || P <- possibilities(Grid, SecondPos), P /= SecondPrev],
    chase(Grid, FirstNext, FirstPos, SecondNext, SecondPos, Count + 1).

possibilities(Grid, Pos = {X, Y}) ->
    case at(Grid, Pos) of
        $- -> [{X + 1, Y}, {X - 1, Y}];
        $| -> [{X, Y + 1}, {X, Y - 1}];
        $7 -> [{X - 1, Y}, {X, Y + 1}];
        $L -> [{X, Y - 1}, {X + 1, Y}];
        $F -> [{X + 1, Y}, {X, Y + 1}];
        $J -> [{X - 1, Y}, {X, Y - 1}]
    end.

possibilities_from_S(Grid, _Pos = {X, Y}) ->
    [
        {XX, YY} ||
            {XX, YY, Cs} <- [
                {X + 1, Y, [$-, $J, $7]},
                {X - 1, Y, [$-, $L, $F]},
                {X, Y + 1, [$|, $L, $J]},
                {X, Y - 1, [$|, $F,$7]}
            ],
            lists:member(at(Grid, {XX, YY}), Cs)
    ].

calculate_enclosure(Grid) ->
    InitialPos = find(Grid, $S),
    [NextPos | _Rest] = possibilities_from_S(Grid, InitialPos),
    Map = #{
        {X, Y} => 0
        ||  X <- lists:seq(0, Grid#grid.width),
            Y <- lists:seq(0, Grid#grid.height),
            {X, Y} /= InitialPos
    },
    walk_enclosure(Grid, NextPos, InitialPos, Map).

walk_enclosure(Grid, Position, PrevPos, Map) ->
    io:format("."),
    NewMap = #{
        {X, Y} => V + angle(Position, PrevPos, GridPos)
        ||  GridPos = {X, Y} := V <- Map,
            {X, Y} /= Position
    },
    case at(Grid, Position) of
        $S ->
            NewMap;
        _ ->
            [NextPos] = [P || P <- possibilities(Grid, Position), P /= PrevPos],
            walk_enclosure(Grid, NextPos, Position, NewMap)
    end.

angle({X1, Y1}, {X2, Y2}, {X0, Y0}) ->
    Theta1 = math:atan2(Y1 - Y0, X1 - X0),
    Theta2 = math:atan2(Y2 - Y0, X2 - X0),
    normalize(Theta2 - Theta1).

normalize(X) ->
    Pi = math:pi(),
    if
        X > Pi -> normalize(X - 2 * Pi);
        X < -1 * Pi -> normalize(X + 2 * Pi);
        true -> X
    end.
