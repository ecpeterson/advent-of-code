-module(day_1).
-export([start_1/1, start_2/1, read_file/1]).

start_1([Filename]) ->
    Lines = read_file(Filename),
    Result = lists:sum([parse_line_1(Line) || Line <- Lines]),
    io:format("Result: ~w~n", [Result]),
    halt(0).

start_2([Filename]) ->
    Lines = read_file(Filename),
    Result = lists:sum([parse_line_2(Line) || Line <- Lines]),
    io:format("Result: ~w~n", [Result]),
    halt(0).

parse_line_1(Line) ->
    parse_line_1(Line, none, none).

parse_line_1([], First, Last) ->
    First * 10 + Last;
parse_line_1([C | Rest], First, Last) when C < $0 orelse C > $9 ->
    parse_line_1(Rest, First, Last);
parse_line_1([C | Rest], First, _Last) ->
    NewLast = list_to_integer([C]),
    NewFirst = case First of none -> NewLast; _ -> First end,
    parse_line_1(Rest, NewFirst, NewLast).

parse_line_2(Line) ->
    parse_line_2(Line, none, none).

parse_line_2([], First, Last) ->
    First * 10 + Last;
parse_line_2("one" ++ _ = [_C | Rest], First, Last) ->
    parse_line_2([$1 | Rest], First, Last);
parse_line_2("two" ++ _ = [_C | Rest], First, Last) ->
    parse_line_2([$2 | Rest], First, Last);
parse_line_2("three" ++ _ = [_C | Rest], First, Last) ->
    parse_line_2([$3 | Rest], First, Last);
parse_line_2("four" ++ _ = [_C | Rest], First, Last) ->
    parse_line_2([$4 | Rest], First, Last);
parse_line_2("five" ++ _ = [_C | Rest], First, Last) ->
    parse_line_2([$5 | Rest], First, Last);
parse_line_2("six" ++ _ = [_C | Rest], First, Last) ->
    parse_line_2([$6 | Rest], First, Last);
parse_line_2("seven" ++ _ = [_C | Rest], First, Last) ->
    parse_line_2([$7 | Rest], First, Last);
parse_line_2("eight" ++ _ = [_C | Rest], First, Last) ->
    parse_line_2([$8 | Rest], First, Last);
parse_line_2("nine" ++ _ = [_C | Rest], First, Last) ->
    parse_line_2([$9 | Rest], First, Last);
parse_line_2([C | Rest], First, Last) when C < $0 orelse C > $9 ->
    parse_line_2(Rest, First, Last);
parse_line_2([C | Rest], First, _Last) ->
    NewLast = list_to_integer([C]),
    NewFirst = case First of none -> NewLast; _ -> First end,
    parse_line_2(Rest, NewFirst, NewLast).

read_file(Filename) when is_list(Filename) ->
    {ok, Handle} = file:open(Filename, read),
    read_file(Handle);
read_file(Handle) ->
    case io:get_line(Handle, none) of
        eof ->
            file:close(Handle),
            [];
        Line ->
            [string:chomp(Line) | read_file(Handle)]
    end.
