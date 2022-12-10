%%%% 2022/day_10.erl

-module(day_10).
-export([start_1/1, start_2/1]).

%%% Entry point for part 1.
-spec start_1([string()]) -> no_return().
start_1([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    TestTimes = [20, 60, 100, 140, 180, 220],
    TestValues = process_instructions(Handle, TestTimes),
    file:close(Handle),
    SumStrength = lists:sum(lists:zipwith(fun erlang:'*'/2, TestTimes, TestValues)),
    io:format("~p~n", [SumStrength]),
    halt(0).

%%% Entry point for part 2.
-spec start_2([string()]) -> no_return().
start_2([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    Width = 40, Height = 6,
    TestTimes = lists:seq(1, Width * Height),
    TestValues = process_instructions(Handle, TestTimes),
    file:close(Handle),
    print_screen(Height, Width, lists:zip(TestTimes, TestValues)),
    halt(0).

%%% See process_instructions/4.
-spec process_instructions(file:io_device(), [integer()]) -> [integer()].
process_instructions(Handle, TestTimes) ->
    Register = 1,
    Cycle = 1,
    process_instructions(Handle, TestTimes, Register, Cycle).

%%% Processes CPU instructions as laid out in a file dump.  Takes a sorted list
%%% of TestTimes at which to sample the X-register and returns a list of samples
%%% in the same order.
-spec process_instructions(file:io_device(), [integer()], integer(), integer()) -> [integer()].
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

%%% Called when the CPU is going to change its value, and samples from the
%%% X-register at the indicated TestTimes up until the moment of change.
-spec change_value([integer()], integer(), integer()) -> {[integer()], [integer()]}.
change_value([TestTime | RestTimes], Register, Cycle) when TestTime < Cycle ->
    {Registers, RemainingTimes} = change_value(RestTimes, Register, Cycle),
    {[Register | Registers], RemainingTimes};
change_value(TestTimes, _Register, _Cycle) ->
    {[], TestTimes}.

%%% Prints the sprite pattern to the screen as described in Part 2.
-spec print_screen(integer(), integer(), [{integer(), integer()}]) -> ok.
print_screen(Height, Width, TestPairs) ->
    lists:foreach(
        fun(RowTests) ->
            lists:foreach(
                fun ({Time, Value}) ->
                    io:format("~c", [painted_char(Width, Time, Value)])
                end,
                RowTests
            ),
            io:format("~n")
        end,
        lists:map(
            fun(Row) -> lists:sublist(TestPairs, 1 + Width * (Row - 1), Width) end,
            lists:seq(1, Height)
        )
    ),
    halt(0).

%%% Calculates whether the sprite overlaps with the CRT painter, based on the
%%% Width of the screen, the Time at which the CRT is working, and the Value of
%%% the CPU's X-register at that time.
-spec painted_char(integer(), integer(), integer()) -> string().
painted_char(Width, Time, Value) ->
    CRTPosition = ((Time - 1) rem Width),
    if
        Value - 1 =< CRTPosition andalso CRTPosition =< Value + 1 -> $#;
        true -> ($ )
    end.
