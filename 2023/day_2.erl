-module(day_2).
-export([start_1/1, start_2/1]).

-define(RED_LIMIT, 12).
-define(GREEN_LIMIT, 13).
-define(BLUE_LIMIT, 14).

start_1([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    Result = process_file_1(Handle),
    io:format("Result: ~w~n", [Result]),
    halt(0).

start_2([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    Result = process_file_2(Handle),
    io:format("Result: ~w~n", [Result]),
    halt(0).

process_file_1(Handle) ->
    process_file_1(Handle, 0).

process_file_1(Handle, Accumulator) ->
    case io:get_line(Handle, none) of
        eof -> file:close(Handle), Accumulator;
        Line ->
            Result = process_line_1(Line),
            process_file_1(Handle, Result + Accumulator)
    end.

process_line_1(Line) ->
    ["Game", IDString | DrawStrings] = string:lexemes(string:chomp(Line), " :;,"),
    ID = list_to_integer(IDString),
    case process_draws(DrawStrings) of
        true -> 0;
        false -> ID
    end.

process_file_2(Handle) ->
    process_file_2(Handle, 0).

process_file_2(Handle, Accumulator) ->
    case io:get_line(Handle, none) of
        eof -> file:close(Handle), Accumulator;
        Line ->
            Result = process_line_2(Line),
            process_file_2(Handle, Result + Accumulator)
    end.

process_line_2(Line) ->
    ["Game", _IDString | DrawStrings] = string:lexemes(string:chomp(Line), " :;,"),
    process_powers(DrawStrings, 0, 0, 0).

process_draws([]) -> false;
process_draws([CountString, "red" | Rest]) ->
    Count = list_to_integer(CountString),
    Count > ?RED_LIMIT orelse process_draws(Rest);
process_draws([CountString, "green" | Rest]) ->
    Count = list_to_integer(CountString),
    Count > ?GREEN_LIMIT orelse process_draws(Rest);
process_draws([CountString, "blue" | Rest]) ->
    Count = list_to_integer(CountString),
    Count > ?BLUE_LIMIT orelse process_draws(Rest).

process_powers([], Reds, Greens, Blues) ->
    Reds * Greens * Blues;
process_powers([C, "red" | Rest], Reds, Greens, Blues) ->
    process_powers(Rest, max(Reds, list_to_integer(C)), Greens, Blues);
process_powers([C, "green" | Rest], Reds, Greens, Blues) ->
    process_powers(Rest, Reds, max(Greens, list_to_integer(C)), Blues);
process_powers([C, "blue" | Rest], Reds, Greens, Blues) ->
    process_powers(Rest, Reds, Greens, max(Blues, list_to_integer(C))).
