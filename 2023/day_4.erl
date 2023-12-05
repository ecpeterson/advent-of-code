-module(day_4).
-export([start_1/1, start_2/1]).

start_1([Filename]) ->
    Lines = day_1:read_file(Filename),
    Result = process_file_1(Lines),
    io:format("Result: ~w~n", [lists:sum(Result)]),
    halt(0).

start_2([Filename]) ->
    Lines = day_1:read_file(Filename),
    PerCardWinners = process_file_2(Lines),
    Result = process_winners(PerCardWinners),
    io:format("Result: ~w~n", [lists:sum(Result)]),
    halt(0).

process_file_1(_Lines = []) ->
    [];
process_file_1(_Lines = [Line | Rest]) ->
    [Left, Right] = string:split(Line, "|"),
    ["Card", _Id | Winners] = string:lexemes(Left, " :"),
    Haves = string:lexemes(Right, " "),
    WinnersWeHave = [X || X <- Haves, lists:member(X, Winners)],
    case WinnersWeHave of
        [] -> process_file_1(Rest);
        _ ->
            Result = 1 bsl (length(WinnersWeHave) - 1),
            [Result | process_file_1(Rest)]
    end.

process_file_2(_Lines = []) ->
    [];
process_file_2(_Lines = [Line | Rest]) ->
    [Left, Right] = string:split(Line, "|"),
    ["Card", _Id | Winners] = string:lexemes(Left, " :"),
    Haves = string:lexemes(Right, " "),
    WinnersWeHave = [X || X <- Haves, lists:member(X, Winners)],
    [length(WinnersWeHave) | process_file_2(Rest)].

process_winners(Winners) ->
    process_winners(Winners, [1 || _ <- Winners]).
process_winners(_Winners = [], _Replicas) ->
    [];
process_winners(_Winners = [Winner | RestWinners], _Replicas = [Replica | RestReplicas]) ->
    {Head, Tail} = lists:split(Winner, RestReplicas),
    NewRestReplicas = lists:append([X + Replica || X <- Head], Tail),
    [Replica | process_winners(RestWinners, NewRestReplicas)].
