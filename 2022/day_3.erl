%%%% 2022/day_3.erl

-module(day_3).
-export([start_1/1, start_2/1]).

start_1([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    Priorities = duplicates(Handle),
    file:close(Handle),
    io:format("~w~n", [lists:sum(lists:map(fun priority/1, Priorities))]),
    halt(0).

start_2([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    Priorities = badges(Handle),
    file:close(Handle),
    io:format("~w~n", [lists:sum(lists:map(fun priority/1, Priorities))]),
    halt(0).

duplicates(Handle) -> duplicates(Handle, []).

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

badges(Handle) -> badges(Handle, []).

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

split_halves(Line) ->
    Length = length(Line),
    FirstHalf = lists:sublist(Line, Length div 2),
    SecondHalf = lists:sublist(Line, Length div 2 + 1, Length div 2),
    {FirstHalf, SecondHalf}.

strip(Line) ->
    string:strip(Line, both, $\n).

priority(Char) when Char >= $a andalso Char =< $z ->
    Char - $a + 1;
priority(Char) when Char >= $A andalso Char =< $Z ->
    Char - $A + 27.
