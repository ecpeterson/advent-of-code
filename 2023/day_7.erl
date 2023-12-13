-module(day_7).
-export([start/1, fix_cards/1, get_rank/1]).

start([Filename]) ->
    Lines = day_1:read_file(Filename),
    Result = process_hands(Lines),
    io:format("Result: ~w~n", [lists:sum(Result)]),
    halt(0).

process_hands(Lines) ->
    ComparableHands = lists:map(
        fun(Line) ->
            [Card1, Card2, Card3, Card4, Card5, $  | BidString] = Line,
            Hand = [Card1, Card2, Card3, Card4, Card5],
            {get_rank(Hand), fix_cards(Hand), list_to_integer(BidString)}
        end,
        Lines
    ),
    SortedHands = lists:sort(ComparableHands),
    [Index * Bid || {Index, {_, _, Bid}} <- lists:enumerate(SortedHands)].

fix_cards(Hand) ->
    lists:map(
        fun (C) when C >= $2 andalso C =< $9 -> C;
            ($T) -> $9 + 1;
            ($J) -> $1;  % $9 + 2 for start_1
            ($Q) -> $9 + 3;
            ($K) -> $9 + 4;
            ($A) -> $9 + 5
        end,
        Hand
    ).

get_rank("JJJJJ") -> -1;
get_rank(Hand) ->
    CardCounts = counts(Hand),
    UnmodifiedCountFrequencies = counts(maps:values(maps:without([$J], CardCounts))),
    JokerFrequency = maps:get($J, CardCounts, 0),
    MaxKey = lists:foldl(fun erlang:max/2, 0, maps:keys(UnmodifiedCountFrequencies)),
    MaxxerKey = MaxKey + JokerFrequency,
    MaxValue = maps:get(MaxKey, UnmodifiedCountFrequencies),
    DroppedFrequencies = UnmodifiedCountFrequencies#{MaxKey := MaxValue - 1},
    MaxxerValue = maps:get(MaxxerKey, DroppedFrequencies, 0),
    CountFrequencies = DroppedFrequencies#{MaxxerKey => MaxxerValue + 1},
    Rank = case CountFrequencies of
        #{5 := 1} -> -1;
        #{4 := 1, 1 := 1} -> -2;
        #{3 := 1, 2 := 1} -> -3;
        #{3 := 1, 1 := 2} -> -4;
        #{2 := 2, 1 := 1} -> -5;
        #{2 := 1, 1 := 3} -> -6;
        _ -> -7
    end,
    Rank.

counts(List) ->
    counts(List, #{}).
counts([], Counts) ->
    Counts;
counts([Item | Rest], Counts) ->
    Count = maps:get(Item, Counts, 0),
    NewCounts = Counts#{Item => Count + 1},
    counts(Rest, NewCounts).
