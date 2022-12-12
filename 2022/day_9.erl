%%%% 2022/day_9.erl

-module(day_9).
-export([start/1]).

-type pos() :: {integer(), integer()}.

%%% Entry point.
-spec start([string()]) -> no_return().
start([Filename, KnotCountStr]) ->
    KnotCount = list_to_integer(string:chomp(KnotCountStr)),
    {ok, Handle} = file:open(Filename, read),
    TailPositions = process_moves(Handle, KnotCount),
    file:close(Handle),
    io:format("~p~n", [ordsets:size(TailPositions)]),
    halt(0).

%%% Read from a file the moves of the head of a multi-segmented rope and returns
%%% a set of positions through which the tail of the rope passes.
-spec process_moves(file:io_device(), integer()) -> ordsets:ordset(integer()).
process_moves(Handle, KnotCount) ->
    Pos = {0, 0},
    TailsPos = lists:duplicate(KnotCount, Pos),
    process_moves(Handle, ordsets:from_list([Pos]), Pos, TailsPos).

%%% Interprets an individual line from the file.
-spec process_moves(file:io_device(), ordsets:ordset(pos()), pos(), [pos()])
    -> ordsets:ordset(pos()).
process_moves(Handle, TailPoses, HeadPos, TailsPos) ->
    case io:get_line(Handle, none) of
        eof ->
            TailPoses;
        [Direction, ($ ) | MoveCountStr] ->
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

%%% Processes an individual move in the simulation.  Together with update_tails,
%%% this moves the head of the rope in the direction of Offset, calculates the
%%% new positions of the later rope segments, and updates the set of positions
%%% the tail has visited.
-spec process_move(pos(), integer(), ordsets:ordset(pos()), pos(), [pos()]) ->
    {ordsets:ordset(pos()), pos(), [pos()]}.
process_move(_, 0, TailPoses, HeadPos, TailsPos) ->
    {TailPoses, HeadPos, TailsPos};
process_move(Offset, Count, TailPoses, _HeadPos = {HeadX, HeadY}, TailsPos) ->
    {DeltaX, DeltaY} = Offset,
    NewHead = {HeadX + DeltaX, HeadY + DeltaY},
    NewTailsPos = update_tails(NewHead, TailsPos),
    NewTail = lists:last(NewTailsPos),
    NewTailPoses = ordsets:add_element(NewTail, TailPoses),
    process_move(Offset, Count - 1, NewTailPoses, NewHead, NewTailsPos).

-spec update_tails(pos(), [pos()]) -> [pos()].
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

%%% signum function.
-spec sign(number()) -> 1 | -1 | 0.
sign(X) when X < 0 -> -1;
sign(X) when X > 0 -> 1;
sign(_) -> 0.
