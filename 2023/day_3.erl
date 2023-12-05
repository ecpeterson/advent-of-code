-module(day_3).
-export([start_1/1, start_2/1]).

start_1([Filename]) ->
    Contents = day_1:read_file(Filename),
    Result = process_file_1(Contents),
    io:format("Result: ~w~n", [Result]),
    halt(0).

start_2([Filename]) ->
    Contents = day_1:read_file(Filename),
    Result = process_file_2(Contents),
    io:format("Result: ~w~n", [Result]),
    halt(0).

process_file_1(Contents) ->
    Operators = find_operators(Contents),
    Parts = find_parts(Contents, Operators),
    lists:sum(Parts).

process_file_2(Contents) ->
    Gears = find_gears(Contents),
    Ratios = find_ratios(Contents, Gears),
    lists:sum([V1 * V2 || _K := [V1, V2] <- Ratios]).

find_operators(Contents) ->
    find_operators(Contents, sets:new([{version, 2}]), 0, 0).
find_operators(_Contents = [], Acc, _Row, _Col) ->
    Acc;
find_operators(_Contents = [[] | OuterRest], Acc, Row, _Col) ->
    find_operators(OuterRest, Acc, Row + 1, 0);
find_operators(_Contents = [[C | InnerRest] | OuterRest], Acc, Row, Col) ->
    NewAcc = if
        C == $. -> Acc;
        C >= $0 andalso C =< $9 -> Acc;
        true -> sets:add_element({Row, Col}, Acc)
    end,
    find_operators([InnerRest | OuterRest], NewAcc, Row, Col + 1).

find_gears(Contents) ->
    find_gears(Contents, #{}, 0, 0).
find_gears(_Contents = [], Acc, _Row, _Col) ->
    Acc;
find_gears(_Contents = [[] | OuterRest], Acc, Row, _Col) ->
    find_gears(OuterRest, Acc, Row + 1, 0);
find_gears(_Contents = [[C | InnerRest] | OuterRest], Acc, Row, Col) ->
    NewAcc = case C of
        $* -> Acc#{{Row, Col} => []};
        _ -> Acc
    end,
    find_gears([InnerRest | OuterRest], NewAcc, Row, Col + 1).

find_parts(Contents, Operators) ->
    find_parts(Contents, Operators, [], 0, 0).
find_parts(_Contents = [], _Operators, Acc, _Row, _Col) ->
    Acc;
find_parts(_Contents = [[] | OuterRest], Operators, Acc, Row, _Col) ->
    find_parts(OuterRest, Operators, Acc, Row + 1, 0);
find_parts(Contents = [[C | InnerRest] | OuterRest], Operators, Acc, Row, Col) ->
    if
        C >= $0 andalso C =< $9 ->
            consume_part(Contents, Operators, Acc, Row, Col);
        true ->
            find_parts([InnerRest | OuterRest], Operators, Acc, Row, Col + 1)
    end.

consume_part([DigitsAndStuff | OuterRest], Operators, Acc, Row, Col) ->
    IsDigit = fun(D) -> D >= $0 andalso D =< $9 end,
    {DigitPrefix, InnerRest} = lists:splitwith(IsDigit, DigitsAndStuff),
    DigitLength = length(DigitPrefix),
    PossiblePositions = sets:from_list(lists:flatten([
        [{Row - 1, C} || C <- lists:seq(Col - 1, Col + DigitLength)],
        [{Row + 1, C} || C <- lists:seq(Col - 1, Col + DigitLength)],
        [{Row, Col - 1}, {Row, Col + DigitLength}]
    ]), [{version, 2}]),
    NewAcc = case sets:is_disjoint(Operators, PossiblePositions) of
        true -> Acc;
        false -> [list_to_integer(DigitPrefix) | Acc]
    end,
    find_parts([InnerRest | OuterRest], Operators, NewAcc, Row, Col + DigitLength).

find_ratios(Contents, Ratios) ->
    find_ratios(Contents, Ratios, 0, 0).
find_ratios(_Contents = [], Ratios, _Row, _Col) ->
    Ratios;
find_ratios(_Contents = [[] | OuterRest], Ratios, Row, _Col) ->
    find_ratios(OuterRest, Ratios, Row + 1, 0);
find_ratios(Contents = [[C | InnerRest] | OuterRest], Ratios, Row, Col) ->
    if
        C >= $0 andalso C =< $9 ->
            consume_part_for_ratio(Contents, Ratios, Row, Col);
        true ->
            find_ratios([InnerRest | OuterRest], Ratios, Row, Col + 1)
    end.

consume_part_for_ratio([DigitsAndStuff | OuterRest], Ratios, Row, Col) ->
    IsDigit = fun(D) -> D >= $0 andalso D =< $9 end,
    {DigitPrefix, InnerRest} = lists:splitwith(IsDigit, DigitsAndStuff),
    DigitLength = length(DigitPrefix),
    Number = list_to_integer(DigitPrefix),
    PossiblePositions = lists:flatten([
        [{Row - 1, C} || C <- lists:seq(Col - 1, Col + DigitLength)],
        [{Row + 1, C} || C <- lists:seq(Col - 1, Col + DigitLength)],
        [{Row, Col - 1}, {Row, Col + DigitLength}]
    ]),
    RatiosToUpdate = maps:with(PossiblePositions, Ratios),
    UpdatedRatios = #{K => [Number | V] || K := V <- RatiosToUpdate},
    NewRatios = maps:merge(Ratios, UpdatedRatios),
    find_ratios([InnerRest | OuterRest], NewRatios, Row, Col + DigitLength).
