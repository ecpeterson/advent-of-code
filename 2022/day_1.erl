%%%% 2022/day_1.erl
%%%%
%%%% Reads \n\n-delimited groups of \n-delimited integers from the text file at
%%%% Filename and prints the sum of the Count largest groups.

-module(day_1).
-export([start/1]).

%%% Entry point for the script.
-spec start([string()]) -> no_return().
start([Filename]) ->
    start([Filename, "1"]);
start([Filename, CountString]) ->
    Count = list_to_integer(CountString),

    %% NOTE: There's a sharp edge here in Erlang where forgetting to destructure
    %%       the Handle from the tuple and then misapplying `io:get_line/1` will
    %%       cause a silent hang rather than a crash.
    {ok, Handle} = file:open(Filename, read),
    MaxElves = top_elves(Handle, Count),

    %% We're requested to print out the sum of the top-scoring elves.
    io:format("~w~n", [lists:sum(MaxElves)]),
    halt(0).

%%% top_elves does the parsing and counting.  top_elves/2 is the friendly entry
%%% point; it builds the defaults for top_elves/3.
-spec top_elves(file:io_device(), integer()) -> [integer()].
top_elves(Handle, Count) ->
    DummyMaxes = [0 || _ <- lists:seq(1, Count)],
    top_elves(Handle, 0, DummyMaxes).

-spec top_elves(file:io_device(), integer(), [integer()]) -> [integer()].
top_elves(Handle, CurrentElf, MaxElvesSoFar) ->
    %% Act on the next line at Handle.
    case io:get_line(Handle, none) of
        %% This closes out all elves, including the current one.
        eof ->
            strip_lowest([CurrentElf | MaxElvesSoFar]);
        %% This closes out the current elf, then recurses to the next one.
        "\n" ->
            top_elves(Handle, 0, strip_lowest([CurrentElf | MaxElvesSoFar]));
        %% Otherwise, this is an integer to assign to the current elf.
        String ->
            %% NOTE: list_to_integer/1 vomits on whitespace
            Integer = list_to_integer(string:strip(String, both, $\n)),
            top_elves(Handle, CurrentElf + Integer, MaxElvesSoFar)
    end.

%%% Utility function: remove the smallest item from List.
-spec strip_lowest([number()]) -> [number()].
strip_lowest(List) ->
    Ascending = lists:sort(List),
    [_Least | Rest] = Ascending,
    Rest.
