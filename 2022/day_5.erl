%%%% 2022/day_5.erl

-module(day_5).
-export([start_1/1, start_2/1]).

-type stacks() :: {[
    integer(), integer(), integer(), integer(), integer(),
    integer(), integer(), integer(), integer()
]}.

%%% Entry point for Part 1.
-spec start_1([string()]) -> no_return().
start_1([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    InitialStacks = list_to_tuple([[] || _ <- lists:seq(1, 9)]),
    LoadedStacks = load_stacks(Handle, InitialStacks),
    MovedStacks = move_9000(Handle, LoadedStacks),
    file:close(Handle),
    Heads = lists:map(fun([C|_]) -> C end, tuple_to_list(MovedStacks)),
    io:format("~s~n", [Heads]),
    halt(0).

%%% Entry point for Part 2.
-spec start_2([string()]) -> no_return().
start_2([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    InitialStacks = list_to_tuple([[] || _ <- lists:seq(1, 9)]),
    LoadedStacks = load_stacks(Handle, InitialStacks),
    MovedStacks = move_9001(Handle, LoadedStacks),
    file:close(Handle),
    Heads = lists:map(fun([C|_]) -> C end, tuple_to_list(MovedStacks)),
    io:format("~s~n", [Heads]),
    halt(0).

%%% Process the start of a data file, with lines that look like
%%%
%%% [A]     [C] [D] [E] [F] [G] [H] [I]
%%%
%%% with optional blanks — in the example, [B] was replaced by a blank.
%%% Returns a tuple of columns, each in vertical-descending order.  Terminates
%%% on the first fully blank line.
%%%
%%% NOTE: This assumes nine columns.
%%% NOTE: Also includes the end-of-column marker at list tail, an ASCII digit.
-spec load_stacks(file:io_device(), stacks()) -> stacks().
load_stacks(Handle, Stacks) ->
    case io:get_line(Handle, none) of
        %% blank line means end of initial input
        "\n" ->
            list_to_tuple(lists:map(fun lists:reverse/1, tuple_to_list(Stacks)));
        String ->
            {_, NewStacks} = lists:foldl(
                fun(_Char, {Position, NewStacks}) when Position rem 4 =/= 2 ->
                    {Position + 1, NewStacks};
                   (_Char = 32, {Position, NewStacks}) ->
                    {Position + 1, NewStacks};
                   (Char, {Position, NewStacks}) ->
                    Index = 1 + ((Position - 2) div 4),
                    Stack = element(Index, NewStacks),
                    {Position + 1, setelement(Index, NewStacks, [Char | Stack])}
                end,
                {1, Stacks},
                String
            ),
            load_stacks(Handle, NewStacks)
    end.

%%% Processes the second half of an input file, with movement instructions that
%%% look like
%%%
%%% move NN from JJ to II
%%%
%%% where NN is a (arbitrary length) number interpreted as a count, and
%%% JJ and II are column indices.
%%%
%%% move_9000 moves blocks one at a time (so that several blocks moved at once
%%% end up in reverse order).
-spec move_9000(file:io_device(), stacks()) -> stacks().
move_9000(Handle, Stacks) ->
    case io:get_line(Handle, none) of
        eof -> Stacks;
        String ->
            ["move", Count, "from", From, "to", To | _] = string:lexemes(String, " \n"),
            NewStacks = move_individual(
                list_to_integer(From), list_to_integer(To), list_to_integer(Count), Stacks
            ),
            move_9000(Handle, NewStacks)
    end.

%%% Moves Count blocks from column From to column To and returns the resulting
%%% rearrangement of Stacks.  Blocks are moved one at a time, so that the blocks
%%% modified by Count > 1 have their vertical order reversed.
-spec move_individual(integer(), integer(), integer(), stacks()) -> stacks().
move_individual(_From, _To, _Count = 0, Stacks) -> Stacks;
move_individual(From, To, Count, Stacks) ->
    [Top | FromStack] = element(From, Stacks),
    ToStack = [Top | element(To, Stacks)],
    NewStacks = setelement(From, setelement(To, Stacks, ToStack), FromStack),
    move_individual(From, To, Count - 1, NewStacks).

%%% move_9001 moves blocks in entire chunks (so that several blocks moved at
%%% once have their order preserved).
-spec move_9001(file:io_device(), stacks()) -> stacks().
move_9001(Handle, Stacks) ->
    case io:get_line(Handle, none) of
        eof -> Stacks;
        String ->
            Lexemes = string:lexemes(String, " \n"),
            ["move", CountStr, "from", FromStr, "to", ToStr | _] = Lexemes,
            [Count, From, To] = lists:map(
                fun erlang:list_to_integer/1,
                [CountStr, FromStr, ToStr]
            ),

            OldFromStack = element(From, Stacks),
            OldToStack = element(To, Stacks),
            {OldFromTop, NewFromStack} = lists:split(OldFromStack, Count),
            NewToStack = OldFromTop ++ OldToStack,

            NewStacks = setelement(From, setelement(To, Stacks, NewToStack), NewFromStack),

            move_9001(Handle, NewStacks)
    end.
