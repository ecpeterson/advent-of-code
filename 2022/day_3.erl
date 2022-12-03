%%%% 2022/day_3.erl

-module(day_3).
-export([start_1/1, start_2/1]).

%%% Entry point for Part 1.
-spec start_1([string()]) -> no_return().
start_1([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    Priorities = duplicates(Handle, []),
    file:close(Handle),
    io:format("~w~n", [lists:sum(lists:map(fun priority/1, Priorities))]),
    halt(0).

%%% Entry point for Part 2.
-spec start_1([string()]) -> no_return().
start_2([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    Priorities = badges(Handle, []),
    file:close(Handle),
    io:format("~w~n", [lists:sum(lists:map(fun priority/1, Priorities))]),
    halt(0).

%%% For each line in Handle, find the unique character which is common to the
%%% first and second halves of the line.
-spec duplicates(file:io_device(), [integer()]) -> [integer()].
duplicates(Handle, Acc) ->
    case io:get_line(Handle, none) of
        eof -> Acc;
        String ->
            Line = strip(String),
            {FirstHalf, SecondHalf} = split_halves(Line),
            [Duplicate] = ordsets:intersection(
                ordsets:from_list(FirstHalf),
                ordsets:from_list(SecondHalf)
            ),
            duplicates(Handle, [Duplicate | Acc])
    end.

%%% For each triple of lines in Handle, find the unique character which is
%%% common to all three lines.
-spec badges(file:io_device(), [integer()]) -> [integer()].
badges(Handle, Acc) ->
    case [io:get_line(Handle, none) || _ <- lists:seq(1, 3)] of
        [eof, eof, eof] -> Acc;
        Lines ->
            [Badge] = ordsets:intersection(lists:map(
                fun(Line) -> ordsets:from_list(strip(Line)) end,
                Lines
            )),
            badges(Handle, [Badge|Acc])
    end.

%%% Divide a string into two halves.
-spec split_halves(string()) -> {string(), string()}.
split_halves(Line) ->
    Length = length(Line),
    FirstHalf = lists:sublist(Line, Length div 2),
    SecondHalf = lists:sublist(Line, Length div 2 + 1, Length div 2),
    {FirstHalf, SecondHalf}.

%%% Remove any carriage returns from a line.
-spec strip(string()) -> string().
strip(Line) ->
    string:strip(Line, both, $\n).

%%% Assigns a priority to a character in A-Za-z.
-spec priority(integer()) -> integer().
priority(Char) when Char >= $a andalso Char =< $z ->
    Char - $a + 1;
priority(Char) when Char >= $A andalso Char =< $Z ->
    Char - $A + 27.
