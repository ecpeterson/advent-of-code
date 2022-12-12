%%%% 2022/day_12.erl

-module(day_12).
-export([start_1/1, start_2/1]).

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
-spec at(grid(), position()) -> integer().
at(Grid = #grid{width = Width, height = Height}, {X, Y})
        when 0 =< X andalso X < Width andalso 0 =< Y andalso Y =< Height ->
    %% NOTE: "+ 1" because of the newline character.
    binary:at(Grid#grid.binary, X + Y * (Grid#grid.width + 1)).

-spec find(grid(), integer()) -> position().
find(_Grid = #grid{binary = Binary, width = Width}, Entry) ->
    {Spot, 1} = binary:match(Binary, <<Entry>>),
    {Spot rem (Width + 1), Spot div (Width + 1)}.

dfs(Grid, Rule, StartPosition, Goal) ->
    dfs(Grid, Rule, Goal, 0, sets:new(), sets:from_list([StartPosition])).

dfs(Grid, Rule, Goal, Step, Visited, ToVisit) ->
    case lists:member(Goal, [at(Grid, X) || X <- sets:to_list(ToVisit)]) of
        true -> Step;
        false ->
            NewVisited = sets:union(Visited, ToVisit),
            NextSteps = sets:from_list(lists:flatten([
                [
                    Shifted ||
                    Shifted = {XX, YY} <- [{X+1, Y}, {X-1, Y}, {X, Y+1}, {X, Y-1}],
                    XX >= 0, YY >= 0, XX < Grid#grid.width, YY < Grid#grid.height,
                    Rule(Grid, Visit, Shifted)
                ] ||
                {X, Y} = Visit <- sets:to_list(ToVisit)
            ])),
            NewToVisit = sets:subtract(NextSteps, NewVisited),
            dfs(Grid, Rule, Goal, Step + 1, NewVisited, NewToVisit)
    end.

%%%
%%% Entry points.
%%%

%%% Entry point for part 1.
-spec start_1([string()]) -> no_return().
start_1([Filename]) ->
    {ok, Binary} = file:read_file(Filename),
    Grid = grid_from_binary(Binary),
    Start = find(Grid, $S),
    Depth = dfs(Grid, fun climb_rule/3, Start, $E),
    io:format("~p~n", [Depth]),
    halt(0).

%%% Entry point for part 2.
-spec start_2([string()]) -> no_return().
start_2([Filename]) ->
    {ok, Binary} = file:read_file(Filename),
    Grid = grid_from_binary(Binary),
    Start = find(Grid, $E),
    Depth = dfs(Grid, fun descend_rule/3, Start, $a),
    io:format("~p~n", [Depth]),
    halt(0).

climb_rule(Grid, From, To) ->
    fixup(at(Grid, From)) + 1 >= fixup(at(Grid, To)).

descend_rule(Grid, To, From) ->
    fixup(at(Grid, From)) + 1 >= fixup(at(Grid, To)).

fixup($S) -> $a;
fixup($E) -> $z;
fixup(X) -> X.
