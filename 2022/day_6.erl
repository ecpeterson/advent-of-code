%%%% 2022/day_6.erl

-module(day_6).
-export([start/1]).

%%% Overall entry point.
-spec start([string()]) -> no_return().
start([Filename, LengthStr]) ->
    Length = list_to_integer(LengthStr),
    {ok, Handle} = file:open(Filename, read),
    String = io:get_line(Handle, ''),
    StartPosition = find_start(String, Length),
    file:close(Handle),
    io:format("~w~n", [StartPosition]),
    halt(0).

%%% Search for the first subsequence of String of length Length with all unique
%%% character.
-spec find_start(string(), integer()) -> integer().
find_start(String, Length) ->
    find_start(String, Length, Length).

-spec find_start(string(), integer(), integer()) -> integer().
find_start(String, Length, Count) ->
    Front = lists:sublist(String, Length),
    case length(ordsets:to_list(ordsets:from_list(Front))) of
        Length -> Count;
        _ ->
            [_|Rest] = String,
            find_start(Rest, Length, Count + 1)
    end.
