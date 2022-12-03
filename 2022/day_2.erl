%%%% 2022/day_2.erl

-module(day_2).
-export([start_1/1, start_2/1]).

-type move() :: integer().     % 0 to 2, where 2 beats 1 beats 0 beats 2.
-type outcome() :: integer().  % 0 lose, 1 draw, 2 win

%%% Entry point for Part 1.
-spec start_1([string()]) -> no_return().
start_1([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    Score = calculate_score_with_moves(Handle, 0),
    file:close(Handle),

    io:format("~w~n", [Score]),
    halt(0).

%%% Entry point for Part 2.
-spec start_2([string()]) -> no_return().
start_2([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    Score = calculate_score_with_strategy(Handle, 0),
    file:close(Handle),

    io:format("~w~n", [Score]),
    halt(0).

%%% Calculate the score of a tournament, where the second column is understood
%%% to record our move.
-spec calculate_score_with_moves(file:io_device(), integer()) -> integer().
calculate_score_with_moves(Handle, Total) ->
    case io:get_line(Handle, none) of
        eof -> Total;
        String ->
            [TheirMoveChar, _, MyMoveChar | _] = String,
            TheirMove = move_from_char(TheirMoveChar),
            MyMove = move_from_char(MyMoveChar - $X + $A),

            Outcome = outcome_from_moves(TheirMove, MyMove),
            GameScore = score_game(MyMove, Outcome),
            calculate_score_with_moves(Handle, Total + GameScore)
    end.

%%% Calculate the score of a tournament, where the second column is understood
%%% to record the game outcome.
-spec calculate_score_with_strategy(file:io_device(), integer()) -> integer().
calculate_score_with_strategy(Handle, Total) ->
    case io:get_line(Handle, none) of
        eof -> Total;
        String ->
            [TheirMoveChar, _, OutcomeChar | _] = String,
            TheirMove = move_from_char(TheirMoveChar),
            Outcome = outcome_from_char(OutcomeChar),

            MyMove = move_from_outcome(TheirMove, Outcome),
            GameScore = score_game(MyMove, Outcome),
            calculate_score_with_strategy(Handle, Total + GameScore)
    end.

%%% Calculate the outcome of a game.
-spec outcome_from_moves(move(), move()) -> outcome().
outcome_from_moves(TheirMove, MyMove) ->
    ((MyMove - TheirMove + 1) + 3) rem 3.

%%% Calculate what move will result in the indicated outcome.
-spec move_from_outcome(move(), outcome()) -> move().
move_from_outcome(TheirMove, Outcome) ->
    (((TheirMove - 1) + (Outcome - 1) + 3) rem 3) + 1.

%%% Deserialize a move.
-spec move_from_char(integer()) -> move().
move_from_char(Char) ->
    Char - $A + 1.

%%% Deserialize an outcome.
-spec outcome_from_char(integer()) -> outcome().
outcome_from_char(Char) ->
    Char - $X.

%%% Compute our score from a game.
-spec score_game(move(), outcome()) -> integer().
score_game(Move, Outcome) ->
    Move + Outcome * 3.
