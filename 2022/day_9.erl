%%%% 2022/day_9.erl

-module(day_9).
-export([start/1]).

%%% Entry point.
-spec start([string()]) -> no_return().
start([Filename, KnotCountStr]) ->
    KnotCount = list_to_integer(string:chomp(KnotCountStr)),
    {ok, Handle} = file:open(Filename, read),
    TailPositions = process_moves(Handle, KnotCount),
    file:close(Handle),
    io:format("~p~n", [ordsets:size(TailPositions)]),
    halt(0).

%%% 
process_moves(Handle, KnotCount) ->
    Pos = {0, 0},
    TailsPos = lists:duplicate(KnotCount, Pos),
    process_moves(Handle, ordsets:from_list([Pos]), Pos, TailsPos).

%%% 
process_moves(Handle, TailPoses, HeadPos, TailsPos) ->
    case io:get_line(Handle, none) of
        eof ->
            TailPoses;
        [Direction, $  | MoveCountStr] ->
            MoveCount = list_to_integer(string:chomp(MoveCountStr)),
            Offset = case Direction of
                $U -> {0, -1};
                $D -> {0, 1};
                $L -> {-1, 0};
                $R -> {1, 0}
            end,
            {NewTailPoses, NewHeadPos, NewTailsPos} =
                process_move(Offset, MoveCount, TailPoses, HeadPos, TailsPos),
            process_moves(Handle, NewTailPoses, NewHeadPos, NewTailsPos)
    end.

%%% 
process_move(_, 0, TailPoses, HeadPos, TailsPos) ->
    {TailPoses, HeadPos, TailsPos};
process_move(Offset, Count, TailPoses, _HeadPos = {HeadX, HeadY}, TailsPos) ->
    {DeltaX, DeltaY} = Offset,
    NewHead = {HeadX + DeltaX, HeadY + DeltaY},
    NewTailsPos = update_tails(NewHead, TailsPos),
    NewTail = lists:last(NewTailsPos),
    NewTailPoses = ordsets:add_element(NewTail, TailPoses),
    process_move(Offset, Count - 1, NewTailPoses, NewHead, NewTailsPos).

update_tails(_Head, []) -> [];
update_tails({NewHeadX, NewHeadY}, [{TailX, TailY} | Rest]) ->
    NewTail = if
        %% If the head is still touching, don't update the tail.
        abs(TailX - NewHeadX) =< 1 andalso abs(TailY - NewHeadY) =< 1 ->
            {TailX, TailY};
        true ->
            {TailX + sign(NewHeadX - TailX), TailY + sign(NewHeadY - TailY)}
    end,
    [NewTail | update_tails(NewTail, Rest)].

%%% 
-spec sign(number()) -> 1 | -1 | 0.
sign(X) when X < 0 -> -1;
sign(X) when X > 0 -> 1;
sign(_) -> 0.
