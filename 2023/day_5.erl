-module(day_5).
-export([start_1/1, start_2/1, apply_entry/2]).

start_1([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    Result = process_file_1(Handle),
    ok = file:close(Handle),
    io:format("Result: ~w~n", [lists:foldl(fun erlang:min/2, infinity, Result)]),
    halt(0).

start_2([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    Result = process_file_2(Handle),
    ok = file:close(Handle),
    io:format("Result: ~w~n", [lists:foldl(fun erlang:min/2, {infinity, infinity}, Result)]),
    halt(0).

process_file_1(Handle) ->
    "seeds: " ++ SeedString = string:chomp(io:get_line(Handle, none)),
    "\n" = io:get_line(Handle, none),
    Seeds = [list_to_integer(S) || S <- string:lexemes(SeedString, " ")],
    process_chunks(Handle, Seeds).

process_file_2(Handle) ->
    "seeds: " ++ SeedString = string:chomp(io:get_line(Handle, none)),
    "\n" = io:get_line(Handle, none),
    Seeds = unflatten(2, [list_to_integer(S) || S <- string:lexemes(SeedString, " ")]),
    process_chunks(Handle, Seeds).

process_chunks(Handle, Inputs) ->
    case io:get_line(Handle, none) of
        eof  -> Inputs;
        _Header ->
            Entries = read_chunk(Handle),
            Outputs = apply_entries(Inputs, Entries),
            process_chunks(Handle, Outputs)
    end.

read_chunk(Handle) ->
    case io:get_line(Handle, none) of
        eof -> [];
        "\n" -> [];
        Line ->
            Entry = list_to_tuple([list_to_integer(X) || X <- string:lexemes(string:chomp(Line), " ")]),
            [Entry | read_chunk(Handle)]
    end.

apply_entries(Inputs, Entries) ->
    {Outputs, Untouched} = lists:foldl(
        fun(Entry, {Outputs, Untouched}) ->
            {NewOutputs, NewUntouched} = apply_entry(Untouched, Entry),
            {NewOutputs ++ Outputs, NewUntouched}
        end,
        {[], Inputs},
        Entries
    ),
    Outputs ++ Untouched.

apply_entry([], _Entry) ->
    {[], []};
apply_entry([Input | Rest], Entry = {Destination, Source, Length})
    when is_integer(Input) andalso Input >= Source andalso Input < Source + Length
->
    NewOutput = Input - Source + Destination,
    {Outputs, Untouched} = apply_entry(Rest, Entry),
    {[NewOutput | Outputs], Untouched};
apply_entry([_Input = {Start, Count} | Rest], Entry = {Destination, Source, Length})
    when Start + Count > Source andalso Start < Source + Length
->
    MeetStart = max(Start, Source),
    MeetEnd = min(Start + Count - 1, Source + Length - 1),
    NewOutput = {MeetStart - Source + Destination, _MeetLength = MeetEnd - MeetStart + 1},
    NewHead = case Source > Start of
        false -> [];
        true  -> [{Start, Source - Start}]
    end,
    NewTail = case Start + Count - 1 > Source + Length - 1 of
        false -> [];
        true  -> [{Source + Length, Start + Count - (Source + Length)}]
    end,
    {Outputs, Untouched} = apply_entry(Rest, Entry),
    {[NewOutput | Outputs], (NewHead ++ NewTail) ++ Untouched};
apply_entry([Input | Rest], Entry) ->
    {Outputs, Untouched} = apply_entry(Rest, Entry),
    {Outputs, [Input | Untouched]}.

unflatten(_Count, _List = []) ->
    [];
unflatten(Count, List) ->
    {Head, Tail} = lists:split(Count, List),
    [list_to_tuple(Head) | unflatten(Count, Tail)].
