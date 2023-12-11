-module(day_6).
-export([start_1/1, start_2/1]).

start_1([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    Result = process_file_1(Handle),
    ok = file:close(Handle),
    io:format("Result: ~w~n", [lists:foldl(fun erlang:'*'/2, 1, Result)]),
    halt(0).

start_2([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    Result = process_file_2(Handle),
    ok = file:close(Handle),
    io:format("Result: ~w~n", [lists:foldl(fun erlang:'*'/2, 1, Result)]),
    halt(0).

process_file_1(Handle) ->
    ["Time:" | Times] = string:lexemes(string:chomp(io:get_line(Handle, none)), " "),
    ["Distance:" | Distances] = string:lexemes(string:chomp(io:get_line(Handle, none)), " "),
    Pairs = lists:zip(
        [list_to_integer(T) || T <- Times],
        [list_to_integer(D) || D <- Distances]
    ),
    [process_pair_1(P) || P <- Pairs].

process_file_2(Handle) ->
    ["Time:" | Times] = string:lexemes(string:chomp(io:get_line(Handle, none)), " "),
    ["Distance:" | Distances] = string:lexemes(string:chomp(io:get_line(Handle, none)), " "),
    Pairs = [{
        list_to_integer(lists:flatten(Times)),
        list_to_integer(lists:flatten(Distances))
    }],
    [process_pair_1(P) || P <- Pairs].

process_pair_1({Total, Record}) ->
    High = round(math:floor((Total + math:sqrt(Total * Total - 4 * Record)) / 2)),
    Low = round(math:ceil((Total - math:sqrt(Total * Total - 4 * Record)) / 2)),
    High - Low + 1.
