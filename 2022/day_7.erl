%%%% 2022/day_7.erl

-module(day_7).
-export([start_1/1, start_2/1]).

%%% Internal representation of a directory tree.
-type dirtree() :: #{string() => (integer() | dirtree())}.

%%% Unix path /a/b/c is represented like ["c", "b", "a"].
-type pwd() :: [string()].

%%% Entry point for part 1.
-spec start_1([string()]) -> no_return().
start_1([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    DirTree = process_terminal_dump(Handle, #{}, []),
    file:close(Handle),
    {_TotalSize, SmallSubdirSizes} = small_subdirs(DirTree, 100000),
    io:format("~p~n", [lists:sum(SmallSubdirSizes)]),
    halt(0).

%%% Entry point for part 2.
-spec start_2([string()]) -> no_return().
start_2([Filename, TotalSpaceStr, NeededSpaceStr]) ->
    TotalSpace = list_to_integer(TotalSpaceStr),
    NeededSpace = list_to_integer(NeededSpaceStr),
    {ok, Handle} = file:open(Filename, read),
    DirTree = process_terminal_dump(Handle, #{}, []),
    file:close(Handle),
    {UsedSpace, _SmallSubdirSizes} = small_subdirs(DirTree, 0),
    SpaceToFree = NeededSpace - (TotalSpace - UsedSpace),
    {UsedSpace, DirSize} = smallest_modification(DirTree, SpaceToFree),
    io:format("~p~n", [DirSize]),
    halt(0).

%%% Builds a dirtree up from a provided terminal dump.
-spec process_terminal_dump(file:io_device(), dirtree(), [string()]) -> dirtree().
process_terminal_dump(Handle, DirTree, PWD) ->
    case io:get_line(Handle, none) of
        eof -> DirTree;
        "$ cd /" ++ _Rest ->
            process_terminal_dump(Handle, DirTree, []);
        "$ ls" ++ _Rest ->
            %% Ignore
            process_terminal_dump(Handle, DirTree, PWD);
        "$ cd .." ++ _Rest ->
            [_Head | Tail] = PWD,
            process_terminal_dump(Handle, DirTree, Tail);
        "$ cd " ++ Rest ->
            process_terminal_dump(Handle, DirTree, [string:chomp(Rest) | PWD]);
        "dir " ++ Rest ->
            NewDirTree = add_directory([string:chomp(Rest) | PWD], DirTree),
            process_terminal_dump(Handle, NewDirTree, PWD);
        String ->
            %% Filesize
            [SizeStr, Filename] = string:lexemes(String, " \n"),
            Size = list_to_integer(SizeStr),
            NewDirTree = add_file(DirTree, PWD, Filename, Size),
            process_terminal_dump(Handle, NewDirTree, PWD)
    end.

%%% Helper function for adding a new directory seen in a listing.
-spec add_directory(pwd(), dirtree()) -> dirtree().
add_directory(_Path = [Last], DirTree) ->
    DirTree#{Last => #{}};
add_directory(Path, DirTree) ->
    Last = lists:last(Path),
    Rest = lists:droplast(Path),
    #{Last := OldSubtree} = DirTree,
    NewSubtree = add_directory(Rest, OldSubtree),
    DirTree#{Last => NewSubtree}.

%%% Helper function for adding a new file seen in a listing.
-spec add_file(dirtree(), pwd(), string(), integer()) -> dirtree().
add_file(DirTree, _PWD = [], Filename, Size) ->
    DirTree#{Filename => Size};
add_file(DirTree, PWD, Filename, Size) ->
    Last = lists:last(PWD),
    Rest = lists:droplast(PWD),
    #{Last := OldSubtree} = DirTree,
    NewSubtree = add_file(OldSubtree, Rest, Filename, Size),
    DirTree#{Last => NewSubtree}.

%%% Calculates a list of subdirectory sizes which fall below a given Limit.
%%% Also returns the total filesize of the system.
-spec small_subdirs(dirtree(), integer()) -> {integer(), [integer()]}.
small_subdirs(DirTree, Limit) ->
    {TotSize, LegalSizes} = maps:fold(
        fun (_Key, Value, {TotSize, LegalSizes}) when is_integer(Value) ->
                {TotSize + Value, LegalSizes};
            (_Key, Value, {TotSize, LegalSizes}) when is_map(Value) ->
                {SubSize, SubLegals} = small_subdirs(Value, Limit),
                {TotSize + SubSize, SubLegals ++ LegalSizes}
        end,
        {0, []}, DirTree
    ),
    case TotSize =< Limit of
        true -> {TotSize, [TotSize | LegalSizes]};
        false -> {TotSize, LegalSizes}
    end.

%%% Calculates the size of the smallest subdirectory which exceeds a given
%%% Target.  Also returns the total filesize of the system (as first value).
-spec smallest_modification(dirtree(), integer()) -> {integer(), integer()}.
smallest_modification(DirTree, Target) ->
    {UsedSpace, BestDirSize} = maps:fold(
        fun (_Key, Value, {UsedSpace, BestDirSize}) when is_integer(Value) ->
                {UsedSpace + Value, BestDirSize};
            (_Key, Value, {UsedSpace, BestDirSize}) when is_map(Value) ->
                {SubSize, SubBest} = smallest_modification(Value, Target),
                {UsedSpace + SubSize, min(SubBest, BestDirSize)}
        end,
        {0, none}, DirTree
    ),
    case UsedSpace >= Target andalso UsedSpace < BestDirSize of
        true -> {UsedSpace, UsedSpace};
        false -> {UsedSpace, BestDirSize}
    end.
