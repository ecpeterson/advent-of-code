-module(day_8).
-export([start_1/1, start_2/1]).

start_1([Filename]) ->
    Lines = day_1:read_file(Filename),
    Result = process_lines_1(Lines),
    io:format("Result: ~w~n", [Result]),
    halt(0).

%%% This is naive and doesn't actually work.  I let it run for 6289832798 iters,
%%% but the actual answer is allegedly ~200x that.  We could period-find
%%% instead, though the answer is easy to calculate only if there's exactly one
%%% exiting position per period.
start_2([Filename]) ->
    Lines = day_1:read_file(Filename),
    Result = process_lines_2(Lines),
    io:format("Result: ~w~n", [Result]),
    halt(0).

process_lines_1([Path, "" | Branches] = _Lines) ->
    Stripped = [string:lexemes(Line, " =(,)") || Line <- Branches],
    Lefts = #{N => L || [N, L, _R] <- Stripped},
    Rights = #{N => R || [N, _L, R] <- Stripped},
    run_1(Path, Lefts, Rights).

process_lines_2([Path, "" | Branches] = _Lines) ->
    Stripped = [string:lexemes(Line, " =(,)") || Line <- Branches],
    Lefts = #{N => L || [N, L, _R] <- Stripped},
    Rights = #{N => R || [N, _L, R] <- Stripped},
    run_2(Path, Lefts, Rights).

run_1(Path, Lefts, Rights) ->
    run_1(Path, Lefts, Rights, "", "AAA", 0).
run_1(Path, Lefts, Rights, "", Current, Count) ->
    run_1(Path, Lefts, Rights, Path, Current, Count);
run_1(_Path, _Lefts, _Rights, _Remainder, "ZZZ", Count) ->
    Count;
run_1(Path, Lefts, Rights, [$L | Remainder], Current, Count) ->
    #{Current := Left} = Lefts,
    run_1(Path, Lefts, Rights, Remainder, Left, Count + 1);
run_1(Path, Lefts, Rights, [$R | Remainder], Current, Count) ->
    #{Current := Right} = Rights,
    run_1(Path, Lefts, Rights, Remainder, Right, Count + 1).

run_2(Path, Lefts, Rights) ->
    StartSet = #{X => [] || X = [_, _, $A] := _V <- Lefts},
    run_2(Path, Lefts, Rights, "", StartSet, 0).
run_2(Path, Lefts, Rights, Remainder, Current, Count) ->
    io:format("\r~w: Current: ~w", [Count, Current]),
    AtEnd = lists:all(fun([_, _, $Z]) -> true; (_) -> false end, maps:keys(Current)),
    case AtEnd of
        true -> Count;
        false -> step(Path, Lefts, Rights, Remainder, Current, Count)
    end.

step(Path, Lefts, Rights, "", Current, Count) ->
    run_2(Path, Lefts, Rights, Path, Current, Count);
step(Path, Lefts, Rights, [$L | Remainder], Current, Count) ->
    NewCurrent = #{maps:get(K, Lefts) => [] || K := [] <- Current},
    run_2(Path, Lefts, Rights, Remainder, NewCurrent, Count + 1);
step(Path, Lefts, Rights, [$R | Remainder], Current, Count) ->
    NewCurrent = #{maps:get(K, Rights) => [] || K := [] <- Current},
    run_2(Path, Lefts, Rights, Remainder, NewCurrent, Count + 1).
