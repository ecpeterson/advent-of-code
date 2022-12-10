%%%% 2022/day_10.erl

-module(day_10).
-export([start_1/1, start_2/1]).

%%% Entry point.
-spec start_1([string()]) -> no_return().
start_1([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    TestTimes = [20, 60, 100, 140, 180, 220],
    TestValues = process_instructions(Handle, TestTimes),
    file:close(Handle),
    SumStrength = lists:sum(lists:zipwith(fun erlang:'*'/2, TestTimes, TestValues)),
    io:format("~p~n", [SumStrength]),
    halt(0).

start_2([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    TestTimes = lists:seq(1, 240),
    TestValues = process_instructions(Handle, TestTimes),
    file:close(Handle),
    TestPairs = lists:zip(TestTimes, TestValues),
    lists:foreach(
        fun({Row, RowTests}) ->
            lists:foreach(
                fun ({Time, Value})
                            when Value - 1 =< Time - (Row - 1) * 40 - 1
                            andalso Time - (Row - 1) * 40 - 1 =< Value + 1 ->
                        io:format("#");
                    ({_Time, _Value}) ->
                        io:format(" ")
                end,
                RowTests
            ),
            io:format("~n")
        end,
        lists:map(
            fun(Row) -> {Row, lists:sublist(TestPairs, 1 + 40 * (Row - 1), 40)} end,
            lists:seq(1, 6)
        )
    ),
    halt(0).

process_instructions(Handle, TestTimes) ->
    Register = 1,
    Cycle = 1,
    process_instructions(Handle, TestTimes, Register, Cycle).

%%% 
process_instructions(Handle, TestTimes, Register, Cycle) ->
    case io:get_line(Handle, none) of
        eof ->
            {TestValues, []} = change_value(TestTimes, Register, Cycle),
            TestValues;
        "noop" ++ _ ->
            process_instructions(Handle, TestTimes, Register, Cycle + 1);
        "addx " ++ ValueStr ->
            Value = list_to_integer(string:chomp(ValueStr)),
            NewRegister = Register + Value,
            {TestValues, RestTimes} = change_value(TestTimes, Register, Cycle + 2),
            TestValues ++ process_instructions(Handle, RestTimes, NewRegister, Cycle + 2)
    end.

change_value([TestTime | RestTimes], Register, Cycle) when TestTime < Cycle ->
    {Registers, RemainingTimes} = change_value(RestTimes, Register, Cycle),
    {[Register | Registers], RemainingTimes};
change_value(TestTimes, _Register, _Cycle) ->
    {[], TestTimes}.
