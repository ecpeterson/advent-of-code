%%%% 2022/day_8.erl

-module(day_8).
-export([start_1/1, start_2/1]).

-type direction() :: north | east | west | south.
-type position() :: {integer(), integer()}.

%%%
%%% Helper type + functions for a rectangular bitmap read out of a file.
%%%

%%% Grid type.
-record(grid, {
    binary :: binary(),
    width :: integer(),
    height :: integer()
}).
-type grid() :: #grid{}.

%%% Interprets a Binary read from a file as a rectangular bitmap.  Each byte is
%%% one entry in the bitmap, and rows are newline-terminated.
-spec grid_from_binary(binary()) -> grid().
grid_from_binary(Binary) ->
    {Width, 1} = binary:match(Binary, <<"\n">>),
    TotalSize = binary:referenced_byte_size(Binary),
    Height = (TotalSize + 1) div (Width + 1),
    #grid{
        binary = Binary,
        width = Width,
        height = Height
    }.

%%% Extracts the bitmap value at the indicated position, cf. binary:at/2.
-spec at(grid(), integer(), integer()) -> integer().
at(Grid = #grid{width = Width, height = Height}, X, Y)
        when 0 =< X andalso X < Width andalso 0 =< Y andalso Y =< Height ->
    %% NOTE: "+ 1" because of the newline character.
    binary:at(Grid#grid.binary, X + Y * (Grid#grid.width + 1)).

%%% 
-spec fold(fun(({integer(), integer()}, integer(), T) -> T), T, grid()) -> T.
fold(F, Acc, Grid = #grid{width = Width, height = Height}) ->
    MapGrid = maps:from_list(lists:flatten([
        [{{X, Y}, at(Grid, X, Y)} || X <- lists:seq(0, Width - 1)]
        || Y <- lists:seq(0, Height - 1)
    ])),
    maps:fold(F, Acc, MapGrid).

%%%
%%% Entry points.
%%%

%%% Entry point for part 1.
-spec start_1([string()]) -> no_return().
start_1([Filename]) ->
    {ok, Binary} = file:read_file(Filename),
    Grid = grid_from_binary(Binary),
    VisibleSet = visible_from_edge(Grid),
    io:format("~p~n", [sets:size(VisibleSet)]),
    %% show_visible_set(VisibleSet),
    halt(0).

%%% Entry point for part 2.
-spec start_2([string()]) -> no_return().
start_2([Filename]) ->
    {ok, Binary} = file:read_file(Filename),
    Grid = grid_from_binary(Binary),
    MaxScore = max_score(Grid),
    io:format("~p~n", [MaxScore]),
    halt(0).

%%%
%%% Part 1
%%%

%%% Calculates the set of trees visible from the edge of the grid.
-spec visible_from_edge(grid()) -> ordsets:ordset(position()).
visible_from_edge(Grid) ->
    lists:foldl(
        fun(Direction, Acc) -> sets:union(visible_from(Grid, Direction), Acc) end,
        sets:new(), [north, south, west, east]
    ).

%%% Calculates the set of trees visible from a particular edge of the grid.
-spec visible_from(grid(), direction()) -> ordsets:ordset(position()).
visible_from(Grid = #grid{width = Width, height = Height}, Direction) ->
    InitialPositions = case Direction of
        north -> [{Column, Height - 1} || Column <- lists:seq(0, Width - 1)];
        south -> [{Column, 0} || Column <- lists:seq(0, Width - 1)];
        west -> [{Width - 1, Row} || Row <- lists:seq(0, Height - 1)];
        east -> [{0, Row} || Row <- lists:seq(0, Height - 1)]
    end,
    lists:foldl(
        fun(InitialPosition, Acc) ->
            sets:union(Acc, walk_line(Grid, Direction, InitialPosition))
        end,
        sets:new(), InitialPositions
    ).

%%% Finds the positions visible while traveling along a particular line.
-spec walk_line(grid(), direction(), position()) -> ordsets:ordset(position()).
walk_line(Grid, Direction, Position) ->
    walk_line(Grid, Direction, Position, -1).

%%% See walk_line/3.
-spec walk_line(grid(), direction(), position(), integer()) -> ordsets:ordset(position()).
walk_line(_, _, {-1, _}, _) -> sets:new();
walk_line(_, _, {_, -1}, _) -> sets:new();
walk_line(#grid{width = Width}, _, {Width, _}, _) -> sets:new();
walk_line(#grid{height = Height}, _, {_, Height}, _) -> sets:new();
%% TODO: could early terminate on Prev == 9.
walk_line(Grid, Direction, Position, Prev) ->
    {X, Y} = Position,
    ThisHeight = at(Grid, X, Y) - $0,
    NewPosition = shift_by(Position, offset(Direction)),
    Continue = walk_line(Grid, Direction, NewPosition, max(Prev, ThisHeight)),
    case ThisHeight > Prev of
        true -> sets:add_element(Position, Continue);
        false -> Continue
    end.

%%%
%%% Part 2
%%%

%%% Calculates the maximum scenic score in the grid.
-spec max_score(grid()) -> integer().
max_score(Grid) ->
    fold(
        fun(Position, _Value, Acc) -> max(this_score(Grid, Position), Acc) end,
        0, Grid
    ).

%%% Calculates the scenic score at a particular point in the grid.
-spec this_score(grid(), position()) -> integer().
this_score(Grid, Position = {X, Y}) ->
    TreeHeight = at(Grid, X, Y) - $0,
    lists:foldl(
        fun(Direction, Acc) ->
            Acc * look_toward(Grid, Position, TreeHeight, Direction)
        end,
        1, [north, east, west, south]
    ).

%%% Counts the number of Grid cells between a given Position and the first
%%% obstruction in a particular Direction, where an obstruction is a cell at the
%%% same TreeHeight.
-spec look_toward(grid(), position(), integer(), direction()) -> integer().
look_toward(_, {0, _}, _, west) -> 0;
look_toward(_, {_, 0}, _, north) -> 0;
look_toward(_Grid = #grid{width = Width}, {X, _}, _, east) when X == Width - 1 -> 0;
look_toward(_Grid = #grid{height = Height}, {_, Y}, _, south) when Y == Height - 1 -> 0;
look_toward(Grid, Position, TreeHeight, Direction) ->
    {NewX, NewY} = NewPosition = shift_by(Position, offset(Direction)),
    NewHeight = at(Grid, NewX, NewY) - $0,
    case NewHeight >= TreeHeight of
        true -> 1;
        false -> 1 + look_toward(Grid, NewPosition, TreeHeight, Direction)
    end.

%%%
%%% Utilities
%%%

%%% Converts a direction atom to a position offset.
-spec offset(direction()) -> position().
offset(north) -> {0, -1};
offset(east) -> {1, 0};
offset(west) -> {-1, 0};
offset(south) -> {0, 1}.

%%% Follows an offset from a source position to return a destination position.
-spec shift_by(position(), position()) -> position().
shift_by(_Position = {X, Y}, _Offset = {DeltaX, DeltaY}) ->
    {X + DeltaX, Y + DeltaY}.

%%%
%%% Debug
%%%

%%% Pretty-prints a VisibleSet.
-spec show_visible_set(ordsets:position()) -> ok.
show_visible_set(VisibleSet) ->
    lists:foreach(
        fun(Row) ->
            lists:foreach(
                fun(Column) ->
                    case sets:is_element({Column, Row}, VisibleSet) of
                        true -> io:format("#");
                        false -> io:format(".")
                    end
                end,
                lists:seq(0, 98)
            ),
            io:format("~n")
        end,
        lists:seq(0, 98)
    ).
