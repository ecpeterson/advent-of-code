%%%% 2022/day_8.erl

-module(day_8).
-export([start_1/1, start_2/1]).

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

at(Grid, X, Y) ->
    %% NOTE: "+ 1" because of the newline character.
    binary:at(Grid#grid.binary, X + Y * (Grid#grid.width + 1)).

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

visible_from_edge(Grid) ->
    lists:foldl(
        fun(Direction, Acc) -> sets:union(visible_from(Grid, Direction), Acc) end,
        sets:new(), [north, south, west, east]
    ).

visible_from(Grid = #grid{width = Width, height = Height}, Direction) ->
    case Direction of
        north ->
            lists:foldl(
                fun(Column, Acc) ->
                    sets:union(Acc, walk_column(Grid, Column, 1, 0, -1))
                end,
                #{}, lists:seq(0, Width - 1)
            );
        south ->
            lists:foldl(
                fun(Column, Acc) ->
                    sets:union(Acc, walk_column(Grid, Column, -1, Height - 1, -1))
                end,
                #{}, lists:seq(0, Width - 1)
            );
        west ->
            lists:foldl(
                fun(Row, Acc) ->
                    sets:union(Acc, walk_row(Grid, Row, 1, 0, -1))
                end,
                #{}, lists:seq(0, Height - 1)
            );
        east ->
            lists:foldl(
                fun(Row, Acc) ->
                    sets:union(Acc, walk_row(Grid, Row, -1, Width - 1, -1))
                end,
                #{}, lists:seq(0, Height - 1)
            )
    end.

walk_column(Grid = #grid{height = Height}, Column, Increment, Position, Prev) ->
    ThisHeight = at(Grid, Column, Position) - $0,
    Continue = case Position + Increment of
        0 -> sets:new();
        Height -> sets:new();
        _ -> walk_column(Grid, Column, Increment, Position + Increment, max(Prev, ThisHeight))
    end,
    case ThisHeight > Prev of
        true -> sets:add_element({Column, Position}, Continue);
        false -> Continue
    end.

walk_row(Grid = #grid{width = Width}, Row, Increment, Position, Prev) ->
    ThisHeight = at(Grid, Position, Row) - $0,
    Continue = case Position + Increment of
        0 -> sets:new();
        Width -> sets:new();
        _ -> walk_row(Grid, Row, Increment, Position+Increment, max(Prev, ThisHeight))
    end,
    case ThisHeight > Prev of
        true -> sets:add_element({Position, Row}, Continue);
        false -> Continue
    end.

%%%
%%% part 2
%%%

max_score(Grid = #grid{width = Width, height = Height}) ->
    lists:foldl(
        fun(X, AccX) ->
            ColMax = lists:foldl(
                fun(Y, AccY) ->
                    max(this_score(Grid, X, Y), AccY)
                end,
                0, lists:seq(0, Height - 1)
            ),
            max(ColMax, AccX)
        end,
        0,
        lists:seq(0, Width - 1)
    ).

this_score(Grid, X, Y) ->
    TreeHeight = at(Grid, X, Y) - $0,
    lists:foldl(
        fun(Direction, Acc) ->
            Acc * look_toward(Grid, X, Y, TreeHeight, Direction)
        end,
        1, [north, east, west, south]
    ).

look_toward(Grid = #grid{width = Width, height = Height}, X, Y, TreeHeight, Direction) ->
    {NewX, NewY} = case Direction of
        north -> {X, Y - 1};
        east -> {X + 1, Y};
        west -> {X - 1, Y};
        south -> {X, Y + 1}
    end,
    if
        NewY < 0 -> 0;
        NewX < 0 -> 0;
        NewX >= Width -> 0;
        NewY >= Height -> 0;
        true ->
            NewHeight = at(Grid, NewX, NewY) - $0,
            %% TODO: check for OOB
            case NewHeight >= TreeHeight of
                true -> 1;
                false -> 1 + look_toward(Grid, NewX, NewY, TreeHeight, Direction)
            end
    end.
