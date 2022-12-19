%%%% 2022/day_15.erl

-module(day_15).
-export([start_1/1, start_2/1]).

intervals_add(Intervals, _Interval = {Low, High}) ->
    {Below, NotBelow} = lists:splitwith(fun({_A, B}) -> B < Low end, Intervals),
    {Meeting, Above} = lists:splitwith(fun({A, _B}) -> A =< High end, NotBelow),
    NewLow = case Meeting of
        [] -> Low;
        [{A, _} | _] -> min(A, Low)
    end,
    NewHigh = case Meeting of
        [] -> High;
        _ ->
            {_, B} = lists:last(Meeting),
            max(B, High)
    end,
    Below ++ [{NewLow, NewHigh}] ++ Above.

-spec start_1([string()]) -> no_return().
start_1([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    {Sensors, Beacons} = process_file(Handle),
    file:close(Handle),

    RowIndex = 2000000,
    RowIntervals = row_coverage(RowIndex, Sensors),

    CoverageSize = lists:sum([1 + Y - X || {X, Y} <- RowIntervals]),
    BeaconsOnRow = length(ordsets:from_list([{X, Y} || {X, Y} <- Beacons, Y == RowIndex])),

    io:format("~p~n", [CoverageSize - BeaconsOnRow]),
    halt(0).

-spec start_2([string()]) -> no_return().
start_2([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    {Sensors, _} = process_file(Handle),
    file:close(Handle),

    {X, Y} = seek_space(Sensors, 0, 4000000, 0, 4000000),

    io:format("~p~n", [X * 4000000 + Y]),
    halt(0).

process_file(Handle) ->
    process_file(Handle, [], []).

process_file(Handle, Sensors, Beacons) ->
    case io:get_line(Handle, none) of
        eof -> {Sensors, Beacons};
        Line ->
            Lexemes = string:lexemes(string:chomp(Line),
                "Sensor at x=, y=: closest beacon is at x=, y="),
            [SensorX, SensorY, BeaconX, BeaconY] = [
                list_to_integer(Z) || Z <- Lexemes],
            Distance = abs(SensorX - BeaconX) + abs(SensorY - BeaconY),
            NewSensors = [{SensorX, SensorY, Distance} | Sensors],
            NewBeacons = [{BeaconX, BeaconY} | Beacons],
            process_file(Handle, NewSensors, NewBeacons)
    end.

row_coverage(RowIndex, Sensors) ->
    row_coverage(RowIndex, Sensors, []).

row_coverage(_RowIndex, _Sensors = [], Intervals) ->
    Intervals;
row_coverage(RowIndex, _Sensors = [{_SX, SY, D} | Rest], Intervals)
    when abs(SY - RowIndex) > D
->
    row_coverage(RowIndex, Rest, Intervals);
row_coverage(RowIndex, _Sensors = [{SX, SY, D} | Rest], Intervals) ->
    Width = D - abs(SY - RowIndex),
    Interval = {SX - Width, SX + Width},
    NewIntervals = intervals_add(Intervals, Interval),
    row_coverage(RowIndex, Rest, NewIntervals).

seek_space(_Sensors, _MinX, _MaxX, MinY, MaxY) when MinY > MaxY -> failure;
seek_space(Sensors, MinX, MaxX, MinY, MaxY) ->
    Intervals = row_coverage(MinY, Sensors),
    case intervals_add(Intervals, {MinX, MaxX}) of
        Intervals ->
            seek_space(Sensors, MinX, MaxX, MinY + 1, MaxY);
        _ ->
            [{X, _} | _] = lists:dropwhile(fun({Start, _End}) -> Start =< MinX end, Intervals),
            {X - 1, MinY}
    end.
