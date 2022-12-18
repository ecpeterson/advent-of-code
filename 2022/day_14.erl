%%%% 2022/day_14.erl

-module(day_14).
-export([start/1]).

-spec start([string()]) -> no_return().
start([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    Occupied = process_file(Handle),
    file:close(Handle),

    Step = flood(Occupied),

    io:format("~p~n", [Step]),
    halt(0).

process_file(Handle) ->
    process_file(Handle, sets:new()).

process_file(Handle, Occupied) ->
    case io:get_line(Handle, none) of
        eof -> Occupied;
        Line ->
            Lexemes = [list_to_integer(Z) || Z <- string:lexemes(Line, ", ->\n")],
            NewOccupied = process_row(Occupied, Lexemes),
            process_file(Handle, NewOccupied)
    end.

process_row(Occupied, [_X1, _Y1]) ->
    Occupied;
process_row(Occupied, [X0, Y0, X1, Y1 | Rest]) ->
    New = if
        X0 == X1 ->
            [{X0, Y} || Y <- lists:seq(min(Y0, Y1), max(Y0, Y1))];
        Y0 == Y1 ->
            [{X, Y0} || X <- lists:seq(min(X0, X1), max(X0, X1))]
    end,
    NewOccupied = sets:union(Occupied, sets:from_list(New)),
    process_row(NewOccupied, [X1, Y1 | Rest]).

flood(Occupied) ->
    Max = lists:foldl(
        fun({_X, Y}, Acc) -> max(Y, Acc) end,
        0,
        sets:to_list(Occupied)
    ),
    %% Part 1
    %% flood(Occupied, Max, 0).
    flood(Occupied, Max, 1).

flood(Occupied, Max, Step) ->
    FirstPos = {500, 0},
    case fall(Occupied, Max, FirstPos) of
        FirstPos -> Step;
        Pos ->
            flood(sets:add_element(Pos, Occupied), Max, Step+1)
    end.

%% Part 1
% fall(_Occupied, Max, {_X, Y}) when Y > Max -> bottom;
% fall(Occupied, Max, {X, Y}) ->
%     Below = not sets:is_element({X, Y+1}, Occupied),
%     Left = not sets:is_element({X-1, Y+1}, Occupied),
%     Right = not sets:is_element({X+1, Y+1}, Occupied),
%     case {Below, Left, Right} of
%         {true, _, _} -> fall(Occupied, Max, {X, Y+1});
%         {_, true, _} -> fall(Occupied, Max, {X-1, Y+1});
%         {_, _, true} -> fall(Occupied, Max, {X+1, Y+1});
%         _ -> {X, Y}
%     end.

fall(_Occupied, Max, {X, Y}) when Y > Max -> {X, Y};
fall(Occupied, Max, {X, Y}) ->
    Below = not sets:is_element({X, Y+1}, Occupied),
    Left = not sets:is_element({X-1, Y+1}, Occupied),
    Right = not sets:is_element({X+1, Y+1}, Occupied),
    case {Below, Left, Right} of
        {true, _, _} -> fall(Occupied, Max, {X, Y+1});
        {_, true, _} -> fall(Occupied, Max, {X-1, Y+1});
        {_, _, true} -> fall(Occupied, Max, {X+1, Y+1});
        _ -> {X, Y}
    end.
