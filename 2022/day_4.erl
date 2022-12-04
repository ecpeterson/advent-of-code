%%%% 2022/day_4.erl

-module(day_4).
-export([start_1/1, start_2/1]).

%%% Entry point for Part 1.
-spec start_1([string()]) -> no_return().
start_1([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    Overlaps = count_contains(Handle, 0),
    file:close(Handle),
    io:format("~w~n", [Overlaps]),
    halt(0).

%%% Entry point for Part 2.
-spec start_2([string()]) -> no_return().
start_2([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    Overlaps = count_overlaps(Handle, 0),
    file:close(Handle),
    io:format("~w~n", [Overlaps]),
    halt(0).

%%% Parse lines like N-M,X-Y and count whether [N, M] either contains or is
%%% contained in [X, Y].
-spec count_contains(file:io_device(), integer()) -> integer().
count_contains(Handle, Overlaps) ->
    case io:get_line(Handle, none) of
        eof -> Overlaps;
        String ->
            [XStart, XEnd, YStart, YEnd] = lists:map(
                fun list_to_integer/1,
                string:lexemes(string:chomp(String), "-,")
            ),

            count_contains(Handle, Overlaps + contains(XStart, XEnd, YStart, YEnd))
    end.

%%% Test whether [N, M] either contains or is contained in [X, Y].
%%% Returns 0 for false, 1 for true.
-spec contains(integer(), integer(), integer(), integer()) -> 0 | 1.
contains(XStart, XEnd, YStart, YEnd) when XStart =< YStart andalso XEnd >= YEnd -> 1;
contains(XStart, XEnd, YStart, YEnd) when YStart =< XStart andalso YEnd >= XEnd -> 1;
contains(_XStart, _XEnd, _YStart, _YEnd) -> 0.

%%% Parse lines like N-M,X-Y and count whether [N, M] intersects [X, Y].
-spec count_overlaps(file:io_device(), integer()) -> integer().
count_overlaps(Handle, Overlaps) ->
    case io:get_line(Handle, none) of
        eof -> Overlaps;
        String ->
            [XStart, XEnd, YStart, YEnd] = lists:map(
                fun list_to_integer/1,
                string:lexemes(string:chomp(String), "-,")
            ),

            count_overlaps(Handle, Overlaps + overlaps(XStart, XEnd, YStart, YEnd))
    end.

%%% Test whether [N, M] intersects [X, Y].
%%% Returns 0 for false, 1 for true.
-spec overlaps(integer(), integer(), integer(), integer()) -> 0 | 1.
overlaps(XStart, XEnd, YStart, YEnd) when XStart < YStart andalso XEnd < YStart -> 0;
overlaps(XStart, XEnd, YStart, YEnd) when YStart < XStart andalso YEnd < XStart -> 0;
overlaps(_XStart, _XEnd, _YStart, _YEnd) -> 1.
