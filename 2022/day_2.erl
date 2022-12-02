%%%% 2022/day_2.erl
%%%%
%%%% 

-module(day_2).
-export([start_1/1, start_2/1]).

%%% Entry point for Part 1.
-spec start_1([string()]) -> no_return().
start_1([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    Score = calculate_score(Handle),

    io:format("~w~n", [Score]),
    halt(0).

%%% Entry point for Part 2.
-spec start_2([string()]) -> no_return().
start_2([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    Score = calculate_score_with_strategy(Handle),

    io:format("~w~n", [Score]),
    halt(0).

-spec calculate_score(file:io_device()) -> integer().
calculate_score(Handle) ->
    calculate_score(Handle, 0).

-spec calculate_score(file:io_device(), integer()) -> integer().
calculate_score(Handle, Total) ->
    case io:get_line(Handle, none) of
        eof -> Total;
        String ->
            [Opponent, _, Move] = string:strip(String, both, $\n),
            OpponentInt = (Opponent - $A) + 1,
            MoveInt = (Move - $X) + 1,

            MoveScore = MoveInt,
            OutcomeScore = 3 * (((MoveInt - OpponentInt + 1) + 3) rem 3),
            calculate_score(Handle, Total + OutcomeScore + MoveScore)
    end.

-spec calculate_score_with_strategy(file:io_device()) -> integer().
calculate_score_with_strategy(Handle) ->
    calculate_score_with_strategy(Handle, 0).

-spec calculate_score_with_strategy(file:io_device(), integer()) -> integer().
calculate_score_with_strategy(Handle, Total) ->
    case io:get_line(Handle, none) of
        eof -> Total;
        String ->
            [Opponent, _, Outcome] = string:strip(String, both, $\n),
            OpponentInt = Opponent - $A,
            OutcomeInt = Outcome - $X,

            MoveInt = ((OpponentInt + (OutcomeInt - 1) + 3) rem 3),
            MoveScore = MoveInt + 1,
            OutcomeScore = OutcomeInt * 3,
            calculate_score_with_strategy(
                Handle,
                Total + OutcomeScore + MoveScore
            )
    end.
