%%%% 2022/day_13.erl

-module(day_13).
-export([start_1/1, start_2/1]).

-type t() :: integer() | [t()].

-spec start_1([string()]) -> no_return().
start_1([Filename]) ->
    {ok, Terms} = file:consult(Filename),
    Misorders = count_misorders(Terms),
    io:format("~p~n", [Misorders]),
    halt(0).

-spec start_2([string()]) -> no_return().
start_2([Filename]) ->
    {ok, Terms} = file:consult(Filename),
    Sorted = lists:sort(fun(A, B) -> less_or_equal(A, B) end, [[[2]], [[6]] | Terms]),
    {value, {First, _}} = lists:keysearch([[2]], 2, lists:enumerate(Sorted)),
    {value, {Second, _}} = lists:keysearch([[6]], 2, lists:enumerate(Sorted)),
    io:format("~p~n", [First * Second]),
    halt(0).

-spec count_misorders([t()]) -> integer().
count_misorders(Terms) ->
    count_misorders(Terms, 1).

-spec count_misorders([t()], integer()) -> integer().
count_misorders([], _Index) -> 0;
count_misorders([A, B | Rest], Index) ->
    C = less_or_equal(A, B),
    case C of
        false -> Index + count_misorders(Rest, Index + 1);
        true -> count_misorders(Rest, Index + 1)
    end.

-spec less_or_equal(t(), t()) -> boolean().
less_or_equal(A, B) when is_integer(A) and is_integer(B) -> A =< B;
less_or_equal(A, B) when is_list(A) and is_integer(B) -> less_or_equal(A, [B]);
less_or_equal(A, B) when is_integer(A) and is_list(B) -> less_or_equal([A], B);
less_or_equal([], B) when is_list(B) -> true;
less_or_equal(A, []) when is_list(A) -> false;
less_or_equal([A | AA], [B | BB]) when A == B ->
    less_or_equal(AA, BB);
less_or_equal([A | _AA], [B | _BB]) ->
    less_or_equal(A, B).
