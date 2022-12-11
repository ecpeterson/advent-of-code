%%%% 2022/day_11.erl

-module(day_11).
-export([start/1]).
-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3
]).

-behavior(gen_server).

-record(monkey, {
    items,
    operation,
    divisibility,
    true_target,
    false_target,
    count = 0,
    monkeys = #{}
}).

%%% Entry point.
-spec start_1([string()]) -> no_return().
start_1([Filename]) ->
    {ok, Handle} = file:open(Filename, read),
    Monkeys = read_monkeys(Handle),
    file:close(Handle),
    maps:map(fun(_K, V) -> boot(V, Monkeys) end, Monkeys),
    lists:foreach(
        fun(_Round) ->
            maps:foreach(fun(_K, V) -> process(V) end, Monkeys)
        end,
        lists:seq(1, 10000)
    ),
    MonkeyValues = lists:map(fun monkey_count/1, maps:values(Monkeys)),
    [First, Second | _] = lists:sort(fun erlang:'>'/2, MonkeyValues),
    io:format("~p~n", [First * Second]),
    halt(0).

%%%
%%% file handling
%%%

read_monkeys(Handle) ->
    read_monkeys(Handle, #{}).

read_monkeys(Handle, Monkeys) ->
    case io:get_line(Handle, none) of
        eof -> Monkeys;
        "\n" -> read_monkeys(Handle, Monkeys);
        "Monkey " ++ MonkeyIDStr ->
            [MonkeyID] = lists:map(fun erlang:list_to_integer/1, string:lexemes(MonkeyIDStr, " :\n")),
            MonkeyData = read_monkey_data(Handle),
            {ok, MonkeyPID} = start(MonkeyData),
            NewMonkeys = Monkeys#{MonkeyID => MonkeyPID},
            read_monkeys(Handle, NewMonkeys)
    end.

read_monkey_data(Handle) ->
    "  Starting items:" ++ ItemsStr = string:chomp(io:get_line(Handle, none)),
    Items = lists:map(fun erlang:list_to_integer/1, string:lexemes(ItemsStr, ", ")),
    "  Operation: new = " ++ OperationsStr = string:chomp(io:get_line(Handle, none)),
    [LeftOperand, OpStr, RightOperand] = string:lexemes(OperationsStr, " "),
    Operation = {
        case OpStr of
            "*" -> fun erlang:'*'/2;
            "+" -> fun erlang:'+'/2
        end,
        case LeftOperand of
            "old" -> LeftOperand;
            _ -> list_to_integer(LeftOperand)
        end,
        case RightOperand of
            "old" -> RightOperand;
            _ -> list_to_integer(RightOperand)
        end
    },
    "  Test: divisible by " ++ DivStr = string:chomp(io:get_line(Handle, none)),
    Divisibility = list_to_integer(DivStr),
    "    If true: throw to monkey " ++ TrueTargetStr = string:chomp(io:get_line(Handle, none)),
    TrueTarget = list_to_integer(TrueTargetStr),
    "    If false: throw to monkey " ++ FalseTargetStr = string:chomp(io:get_line(Handle, none)),
    FalseTarget = list_to_integer(FalseTargetStr),
    #monkey{
        items = Items,
        operation = Operation,
        divisibility = Divisibility,
        true_target = TrueTarget,
        false_target = FalseTarget
    }.

%%%
%%% message interface to monkey process
%%%

monkey_count(Monkey) ->
    gen_server:call(Monkey, count).

boot(Monkey, Monkeys) ->
    gen_server:cast(Monkey, { boot, Monkeys }).

start(MonkeyData) ->
    gen_server:start(?MODULE, [MonkeyData], []).

process(MonkeyPID) ->
    gen_server:call(MonkeyPID, process).

toss(MonkeyPID, Item) ->
    gen_server:call(MonkeyPID, {toss, Item}).

%%%
%%% gen_server callbacks
%%%

init([MonkeyData]) ->
    {ok, MonkeyData}.

handle_call(count, _From, State) ->
    {reply, State#monkey.count, State};

handle_call({toss, Item}, _From, State = #monkey{items = Items}) ->
    {reply, ok, State#monkey{items = Items ++ [Item]}};

handle_call(process, _From, State = #monkey{count = Count, items = Items}) ->
    lists:foreach(fun(Item) -> process_item(State, Item) end, Items),
    {reply, ok, State#monkey{items = [], count = Count + length(Items)}}.

handle_cast({boot, Monkeys}, State) ->
    {noreply, State#monkey{monkeys = Monkeys}}.

handle_info(_Message, State) ->
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%% utilities
%%%

process_item(State, Worry) ->
    % NewWorry = apply_function(State#monkey.operation, Worry) div 3,
    NewWorry = apply_function(State#monkey.operation, Worry) rem (5 * 7 * 17 * 13 * 19 * 3 * 11 * 2),
    #{
        State#monkey.true_target := TruePID,
        State#monkey.false_target := FalsePID
    } = State#monkey.monkeys,
    case NewWorry rem State#monkey.divisibility of
        0 -> toss(TruePID, NewWorry);
        _ -> toss(FalsePID, NewWorry)
    end.

apply_function({Operation, LeftOperand, RightOperand}, Old) ->
    L = case LeftOperand of "old" -> Old; _ -> LeftOperand end,
    R = case RightOperand of "old" -> Old; _ -> RightOperand end,
    Operation(L, R).
