-module(day_9).
-export([start_1/1, start_2/1]).

start_1([Filename]) ->
    Lines = day_1:read_file(Filename),
    Results = [process_line_1(Line) || Line <- Lines],
    io:format("Result: ~w~n", [lists:foldl(fun erlang:'+'/2, 0, Results)]),
    halt(0).

start_2([Filename]) ->
    Lines = day_1:read_file(Filename),
    Results = [process_line_2(Line) || Line <- Lines],
    io:format("Result: ~w~n", [lists:foldl(fun erlang:'+'/2, 0, Results)]),
    halt(0).

process_line_1(Line) ->
    Integers = [list_to_integer(I) || I <- string:lexemes(Line, " ")],
    extrapolate(Integers).

process_line_2(Line) ->
    Integers = [list_to_integer(I) || I <- string:lexemes(Line, " ")],
    extrapolate_back(Integers).

extrapolate(Integers) ->
    case lists:all(fun(V) -> V =:= 0 end, Integers) of
        true  -> 0;
        false ->
            Differences = [Y - X || {X, Y} <- lists:zip(
                lists:droplast(Integers),
                lists:nthtail(1, Integers)
            )],
            lists:last(Integers) + extrapolate(Differences)
    end.

extrapolate_back(Integers) ->
    case lists:all(fun(V) -> V =:= 0 end, Integers) of
        true  -> 0;
        false ->
            Differences = [Y - X || {X, Y} <- lists:zip(
                lists:droplast(Integers),
                lists:nthtail(1, Integers)
            )],
            lists:nth(1, Integers) - extrapolate_back(Differences)
    end.
