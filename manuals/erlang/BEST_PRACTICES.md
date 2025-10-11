# Erlang Best Practices

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Target Audience**: AI Agents developing Erlang projects  
**Erlang/OTP Version**: 24+

---

## Table of Contents

1. [Erlang Idioms](#erlang-idioms)
2. [Anti-Patterns](#anti-patterns)
3. [Performance Optimization](#performance-optimization)
4. [Security Best Practices](#security-best-practices)
5. [Error Handling](#error-handling)
6. [Concurrency Patterns](#concurrency-patterns)
7. [OTP Patterns](#otp-patterns)
8. [Code Organization](#code-organization)
9. [Testing Best Practices](#testing-best-practices)
10. [Common Gotchas](#common-gotchas)

---

## Erlang Idioms

### 1. Pattern Matching in Function Heads

```erlang
%% ❌ BAD: Using if/case when pattern matching works
process_message(Message) ->
    if
        Message == start -> handle_start();
        Message == stop -> handle_stop();
        true -> handle_unknown()
    end.

%% ✅ GOOD: Pattern matching in function heads
process_message(start) -> handle_start();
process_message(stop) -> handle_stop();
process_message(_) -> handle_unknown().

%% ✅ GOOD: With guards
is_valid_age(Age) when is_integer(Age), Age >= 0, Age =< 120 ->
    true;
is_valid_age(_) ->
    false.
```

### 2. Use Records for Structured Data

```erlang
%% ❌ BAD: Tuples for complex data
User = {john, 30, "john@example.com", active}.
{Name, Age, Email, Status} = User.  % Fragile

%% ✅ GOOD: Records
-record(user, {
    name :: binary(),
    age :: non_neg_integer(),
    email :: binary(),
    status = active :: active | inactive
}).

User = #user{name = <<"John">>, age = 30, email = <<"john@example.com">>}.
Name = User#user.name.  % Clear and typed
UpdatedUser = User#user{age = 31}.  % Easy to update
```

### 3. Use List Comprehensions

```erlang
%% ❌ BAD: Manual list building
double_list(List) ->
    double_list(List, []).

double_list([], Acc) ->
    lists:reverse(Acc);
double_list([H|T], Acc) ->
    double_list(T, [H*2|Acc]).

%% ✅ GOOD: List comprehension
double_list(List) ->
    [X*2 || X <- List].

%% ✅ GOOD: With filter
even_squares(List) ->
    [X*X || X <- List, X rem 2 =:= 0].
```

### 4. Use Tail Recursion

```erlang
%% ❌ BAD: Non-tail recursive (stack overflow for large lists)
sum([]) -> 0;
sum([H|T]) -> H + sum(T).

%% ✅ GOOD: Tail recursive with accumulator
sum(List) -> sum(List, 0).

sum([], Acc) -> Acc;
sum([H|T], Acc) -> sum(T, Acc + H).

%% ✅ BETTER: Use lists:foldl/3
sum(List) -> lists:foldl(fun(X, Acc) -> X + Acc end, 0, List).
```

### 5. Use Binary Syntax

```erlang
%% ❌ BAD: String concatenation
build_message(Name, Age) ->
    "Hello " ++ Name ++ ", you are " ++ integer_to_list(Age).

%% ✅ GOOD: Binary construction
build_message(Name, Age) ->
    <<"Hello ", Name/binary, ", you are ", (integer_to_binary(Age))/binary>>.

%% ✅ GOOD: iolist (efficient for I/O)
build_message(Name, Age) ->
    ["Hello ", Name, ", you are ", integer_to_list(Age)].
```

### 6. Use maps for Key-Value Data

```erlang
%% ❌ BAD: proplist for complex data
User = [{name, <<"John">>}, {age, 30}, {email, <<"john@example.com">>}].
Name = proplists:get_value(name, User).  % Linear search

%% ✅ GOOD: Maps (O(log n) lookup)
User = #{name => <<"John">>, age => 30, email => <<"john@example.com">>}.
Name = maps:get(name, User).

%% ✅ GOOD: Pattern matching maps
process_user(#{name := Name, age := Age}) ->
    io:format("Name: ~s, Age: ~p~n", [Name, Age]).
```

### 7. Use `=:=` Instead of `==`

```erlang
%% ❌ AVOID: Type-converting comparison
1 == 1.0  % true (converts types)

%% ✅ GOOD: Exact comparison
1 =:= 1.0  % false (no type conversion)
1 =:= 1    % true

%% Use =:= for most cases, == only when you need type conversion
```

### 8. Use Anonymous Functions

```erlang
%% ✅ GOOD: Anonymous functions
Double = fun(X) -> X * 2 end.
lists:map(Double, [1, 2, 3]).

%% ✅ GOOD: Inline
lists:map(fun(X) -> X * 2 end, [1, 2, 3]).

%% ✅ GOOD: With multiple clauses
Process = fun
    ({ok, Value}) -> {processed, Value};
    ({error, _} = Error) -> Error
end.
```

### 9. Use `andalso`/`orelse` for Short-Circuit

```erlang
%% ❌ BAD: `and` evaluates both sides
is_valid(User) ->
    User =/= undefined and User#user.age >= 18.  % Crashes if User is undefined

%% ✅ GOOD: `andalso` short-circuits
is_valid(User) ->
    User =/= undefined andalso User#user.age >= 18.

%% ✅ GOOD: `orelse` for defaults
get_port(Config) ->
    maps:get(port, Config, undefined) orelse 8080.
```

### 10. Use Process Dictionary Sparingly

```erlang
%% ❌ BAD: Overusing process dictionary
put(counter, 0),
put(state, initial),
put(config, load_config()),
% Process dictionary becomes hidden state

%% ✅ GOOD: Explicit state passing
loop(State = #{counter := Counter, config := Config}) ->
    receive
        increment ->
            loop(State#{counter => Counter + 1});
        _ ->
            loop(State)
    end.
```

---

## Anti-Patterns

### 1. Defensive Programming (Too Much Error Checking)

```erlang
%% ❌ BAD: Defensive programming everywhere
process_data(Data) ->
    if
        Data =:= undefined -> {error, undefined_data};
        not is_binary(Data) -> {error, invalid_type};
        byte_size(Data) =:= 0 -> {error, empty_data};
        byte_size(Data) > 1000000 -> {error, too_large};
        true ->
            case validate_format(Data) of
                false -> {error, invalid_format};
                true ->
                    try
                        process(Data)
                    catch
                        _:_ -> {error, processing_failed}
                    end
            end
    end.

%% ✅ GOOD: Let it crash
process_data(Data) when is_binary(Data), byte_size(Data) > 0 ->
    %% Supervisor will restart if it crashes
    process(Data).
```

### 2. Using Process Dictionary as State

```erlang
%% ❌ BAD: Hidden state in process dictionary
handle_request(Request) ->
    State = get(state),
    NewState = process(Request, State),
    put(state, NewState),
    ok.

%% ✅ GOOD: Explicit state
loop(State) ->
    receive
        {request, Request, From} ->
            NewState = process(Request, State),
            From ! {response, ok},
            loop(NewState)
    end.
```

### 3. Not Using OTP Behaviors

```erlang
%% ❌ BAD: Manual process implementation
start() ->
    spawn(fun() -> loop([]) end).

loop(State) ->
    receive
        {get, From} ->
            From ! {ok, State},
            loop(State);
        {set, NewState} ->
            loop(NewState);
        stop ->
            ok
    end.

%% ✅ GOOD: Use gen_server
-module(my_server).
-behaviour(gen_server).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, []}.

handle_call(get, _From, State) ->
    {reply, {ok, State}, State};
handle_call({set, NewState}, _From, _State) ->
    {reply, ok, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% ... other callbacks
```

### 4. Ignoring Return Values

```erlang
%% ❌ BAD: Ignoring return values
gen_server:call(Server, Request),
file:write(File, Data),
% No error checking

%% ✅ GOOD: Pattern match on results
case gen_server:call(Server, Request) of
    {ok, Result} -> handle_success(Result);
    {error, Reason} -> handle_error(Reason)
end.

%% ✅ GOOD: Let it crash if should succeed
{ok, Result} = gen_server:call(Server, Request),
ok = file:write(File, Data).
```

### 5. Long-Running gen_server Calls

```erlang
%% ❌ BAD: Blocking gen_server with slow operation
handle_call(slow_operation, _From, State) ->
    timer:sleep(10000),  % Blocks entire server!
    {reply, ok, State}.

%% ✅ GOOD: Spawn worker or use cast
handle_call(slow_operation, From, State) ->
    spawn(fun() ->
        Result = do_slow_operation(),
        gen_server:reply(From, Result)
    end),
    {noreply, State}.

%% ✅ BETTER: Use async worker pool
handle_call(slow_operation, From, State = #{pool := Pool}) ->
    poolboy:transaction(Pool, fun(Worker) ->
        gen_server:reply(From, gen_server:call(Worker, slow_operation))
    end),
    {noreply, State}.
```

### 6. Not Using Proper Supervision

```erlang
%% ❌ BAD: No supervision
start() ->
    spawn(fun worker_loop/0).

%% ✅ GOOD: Supervised process
-module(my_sup).
-behaviour(supervisor).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    ChildSpecs = [
        #{id => worker,
          start => {my_worker, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker}
    ],
    {ok, {SupFlags, ChildSpecs}}.
```

### 7. Mixing Business Logic with Infrastructure

```erlang
%% ❌ BAD: gen_server doing business logic
handle_call({process_order, Order}, _From, State) ->
    %% Validate order
    case validate(Order) of
        ok ->
            %% Calculate total
            Total = calculate_total(Order),
            %% Save to database
            db:save(orders, Order),
            %% Send email
            send_confirmation_email(Order),
            %% Update inventory
            update_inventory(Order),
            {reply, {ok, Total}, State};
        Error ->
            {reply, Error, State}
    end.

%% ✅ GOOD: Separate concerns
handle_call({process_order, Order}, _From, State) ->
    %% Delegate to domain logic
    Result = order_processor:process(Order),
    {reply, Result, State}.
```

---

## Performance Optimization

### 1. Use Binaries for Large Data

```erlang
%% ❌ BAD: Lists for large data
LargeData = lists:duplicate(1000000, $a).  % Huge memory

%% ✅ GOOD: Binaries
LargeData = binary:copy(<<$a>>, 1000000).  % Efficient

%% ✅ GOOD: Binary comprehension
Data = << <<X>> || X <- lists:seq(1, 1000000) >>.
```

### 2. Use ETS for In-Memory Storage

```erlang
%% ❌ BAD: Process-based storage (bottleneck)
store_loop(Map) ->
    receive
        {put, Key, Value, From} ->
            From ! ok,
            store_loop(maps:put(Key, Value, Map));
        {get, Key, From} ->
            From ! maps:get(Key, Map, undefined),
            store_loop(Map)
    end.

%% ✅ GOOD: ETS table (concurrent access)
Table = ets:new(storage, [set, public, named_table]),
ets:insert(storage, {Key, Value}),
[{_, Value}] = ets:lookup(storage, Key).
```

### 3. Avoid String Concatenation

```erlang
%% ❌ BAD: String/list concatenation
Result = "Header" ++ Data ++ "Footer".  % Copies data

%% ✅ GOOD: iolist
Result = ["Header", Data, "Footer"].  % No copying

%% ✅ GOOD: Binary
Result = <<"Header", Data/binary, "Footer">>.
```

### 4. Use Efficient Data Structures

```erlang
%% ❌ BAD: List for frequent lookups
Members = [john, jane, bob, alice],
lists:member(bob, Members).  % O(n)

%% ✅ GOOD: Set for membership testing
Members = sets:from_list([john, jane, bob, alice]),
sets:is_element(bob, Members).  % O(log n)

%% ✅ BETTER: Map for key-value
Members = #{john => true, jane => true, bob => true, alice => true},
maps:is_key(bob, Members).  % O(log n)
```

### 5. Spawn Processes Efficiently

```erlang
%% ❌ BAD: Spawn many processes sequentially
Pids = [spawn(fun() -> worker(X) end) || X <- lists:seq(1, 10000)].

%% ✅ GOOD: Use process pool
Pool = poolboy:start_link([{size, 100}, {max_overflow, 200}]),
[poolboy:transaction(Pool, fun(Worker) -> 
    gen_server:call(Worker, {work, X})
end) || X <- lists:seq(1, 10000)].
```

### 6. Use Native Functions

```erlang
%% ❌ SLOW: Manual implementation
sum_list([]) -> 0;
sum_list([H|T]) -> H + sum_list(T).

%% ✅ FAST: Native BIF
sum_list(List) -> lists:sum(List).

%% ✅ FAST: Fold
sum_list(List) -> lists:foldl(fun(X, Acc) -> X + Acc end, 0, List).
```

### 7. Profile Before Optimizing

```erlang
%% Profile with fprof
fprof:trace(start),
Result = expensive_function(),
fprof:trace(stop),
fprof:profile(),
fprof:analyse([{dest, "profile.txt"}]).

%% Profile with eprof
eprof:start(),
eprof:start_profiling([self()]),
expensive_function(),
eprof:stop_profiling(),
eprof:analyze(total).
```

---

## Security Best Practices

### 1. Validate All Input

```erlang
%% ✅ GOOD: Input validation
validate_user_input(Input) ->
    case Input of
        #{<<"username">> := Username, <<"age">> := Age}
          when is_binary(Username), byte_size(Username) > 0, byte_size(Username) < 100,
               is_integer(Age), Age >= 0, Age =< 120 ->
            {ok, #{username => Username, age => Age}};
        _ ->
            {error, invalid_input}
    end.
```

### 2. Use Parameterized Queries

```erlang
%% ❌ BAD: SQL injection vulnerability
get_user(Email) ->
    Query = "SELECT * FROM users WHERE email = '" ++ Email ++ "'",
    epgsql:query(Conn, Query).

%% ✅ GOOD: Parameterized query
get_user(Email) ->
    Query = "SELECT * FROM users WHERE email = $1",
    epgsql:equery(Conn, Query, [Email]).
```

### 3. Sanitize Output

```erlang
%% ✅ GOOD: HTML escaping
escape_html(Binary) ->
    binary:replace(Binary, [<<"<">>, <<">">>, <<"&">>, <<"\"">>, <<"'">>],
                   [<<"&lt;">>, <<"&gt;">>, <<"&amp;">>, <<"&quot;">>, <<"&#39;">>],
                   [global]).
```

### 4. Limit Resource Usage

```erlang
%% ✅ GOOD: Limit message queue
loop(State) ->
    {message_queue_len, QLen} = process_info(self(), message_queue_len),
    if
        QLen > 10000 ->
            %% Drop messages or slow down
            timer:sleep(100);
        true ->
            ok
    end,
    receive
        Msg -> handle_message(Msg, State)
    after
        0 -> loop(State)
    end.
```

### 5. Use Secure Defaults

```erlang
%% ✅ GOOD: Secure SSL options
ssl_options() ->
    [
        {versions, ['tlsv1.2', 'tlsv1.3']},
        {verify, verify_peer},
        {fail_if_no_peer_cert, true},
        {ciphers, [
            "ECDHE-ECDSA-AES256-GCM-SHA384",
            "ECDHE-RSA-AES256-GCM-SHA384"
        ]},
        {honor_cipher_order, true}
    ].
```

---

## Error Handling

### 1. Let It Crash Philosophy

```erlang
%% ✅ GOOD: Let supervisor handle failures
process_request(Request) when is_map(Request) ->
    %% Will crash if Request doesn't have required fields
    %% Supervisor restarts the process
    #{action := Action, data := Data} = Request,
    perform_action(Action, Data).
```

### 2. Use Links and Monitors

```erlang
%% ✅ GOOD: Link processes that depend on each other
start_linked_workers() ->
    Pid1 = spawn_link(fun() -> worker1() end),
    Pid2 = spawn_link(fun() -> worker2() end),
    {Pid1, Pid2}.

%% ✅ GOOD: Monitor for notification
start_monitored_worker() ->
    {Pid, Ref} = spawn_monitor(fun() -> worker() end),
    receive
        {'DOWN', Ref, process, Pid, Reason} ->
            {error, Reason}
    after
        5000 ->
            {ok, Pid}
    end.
```

### 3. Design Supervision Trees

```erlang
%% ✅ GOOD: Proper supervision hierarchy
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,  % Max 5 restarts
        period => 10     % In 10 seconds
    },
    
    ChildSpecs = [
        %% Database connection pool
        #{id => db_pool,
          start => {db_pool, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker},
        
        %% API server
        #{id => api_server,
          start => {api_server, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker},
        
        %% Worker supervisor
        #{id => worker_sup,
          start => {worker_sup, start_link, []},
          restart => permanent,
          shutdown => infinity,
          type => supervisor}
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
```

### 4. Use Try-Catch Appropriately

```erlang
%% ✅ GOOD: Try-catch for expected errors only
safe_parse(Binary) ->
    try
        jiffy:decode(Binary)
    catch
        throw:Error ->
            {error, {invalid_json, Error}};
        error:Error ->
            {error, {parse_error, Error}}
    end.

%% ✅ GOOD: After clause for cleanup
with_file(Filename, Fun) ->
    {ok, File} = file:open(Filename, [read]),
    try
        Fun(File)
    after
        file:close(File)
    end.
```

---

## Concurrency Patterns

### 1. Message Passing

```erlang
%% ✅ GOOD: Clear message protocol
-type request() :: {get, key(), pid()} | {put, key(), value(), pid()}.
-type response() :: {ok, value()} | {error, term()}.

loop(State) ->
    receive
        {get, Key, From} ->
            Response = case maps:find(Key, State) of
                {ok, Value} -> {ok, Value};
                error -> {error, not_found}
            end,
            From ! Response,
            loop(State);
        
        {put, Key, Value, From} ->
            NewState = maps:put(Key, Value, State),
            From ! ok,
            loop(NewState)
    end.
```

### 2. Worker Pool Pattern

```erlang
%% ✅ GOOD: Controlled parallelism
process_batch(Items, PoolSize) ->
    Pool = create_worker_pool(PoolSize),
    Results = pmap(fun process_item/1, Items, Pool),
    shutdown_pool(Pool),
    Results.

pmap(Fun, List, Pool) ->
    Parent = self(),
    Refs = [begin
        Ref = make_ref(),
        poolboy:transaction(Pool, fun(Worker) ->
            spawn(fun() ->
                Result = Fun(Item),
                Parent ! {Ref, Result}
            end)
        end),
        Ref
    end || Item <- List],
    
    [receive {Ref, Result} -> Result end || Ref <- Refs].
```

### 3. Timeout Patterns

```erlang
%% ✅ GOOD: Always use timeouts
call_with_timeout(Server, Request, Timeout) ->
    Server ! {self(), Request},
    receive
        {Server, Response} ->
            {ok, Response}
    after
        Timeout ->
            {error, timeout}
    end.

%% ✅ GOOD: Retry with backoff
retry_call(Server, Request, MaxRetries) ->
    retry_call(Server, Request, MaxRetries, 100).

retry_call(_Server, _Request, 0, _Backoff) ->
    {error, max_retries};
retry_call(Server, Request, Retries, Backoff) ->
    case call_with_timeout(Server, Request, 5000) of
        {ok, Response} ->
            {ok, Response};
        {error, timeout} ->
            timer:sleep(Backoff),
            retry_call(Server, Request, Retries - 1, Backoff * 2)
    end.
```

---

## OTP Patterns

### 1. Gen_Server State Management

```erlang
%% ✅ GOOD: Clean state management
-record(state, {
    connections = #{} :: #{pid() => connection()},
    config :: config(),
    stats = #{} :: stats()
}).

handle_call({add_connection, Conn}, _From, State = #state{connections = Conns}) ->
    NewConns = maps:put(self(), Conn, Conns),
    {reply, ok, State#state{connections = NewConns}}.
```

### 2. Application Structure

```erlang
%% ✅ GOOD: Proper application callback
-module(myapp_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    %% Start top-level supervisor
    case myapp_sup:start_link() of
        {ok, Pid} ->
            %% Initialize application resources
            myapp_config:load(),
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    %% Cleanup application resources
    myapp_config:save(),
    ok.
```

### 3. Hot Code Loading

```erlang
%% ✅ GOOD: Support code upgrades
code_change(_OldVsn, State, _Extra) ->
    %% Migrate state if needed
    {ok, State}.

%% In gen_server loop
handle_info({'$gen_call', From, {upgrade, Module}}, State) ->
    code:purge(Module),
    code:load_file(Module),
    gen_server:reply(From, ok),
    {noreply, State}.
```

---

## Code Organization

### 1. Module Organization

```erlang
%%% @doc
%%% Server module description
%%% @end

-module(myapp_server).
-behaviour(gen_server).

%%% API
-export([start_link/0, stop/0]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% Internal exports (for testing)
-export([]).

%%% Types
-type state() :: #{atom() => term()}.

%%% Records
-record(state, {}).

%%% Macros
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%%===================================================================
%%% Internal functions
%%%===================================================================
```

### 2. Use Header Files

```erlang
%% include/myapp.hrl
-record(user, {
    id :: integer(),
    name :: binary(),
    email :: binary()
}).

-type user() :: #user{}.

%% In module
-include("myapp.hrl").
```

### 3. Separate Concerns

```erlang
%% ✅ GOOD: Domain logic separate from infrastructure
-module(order_service).
-export([create_order/1, cancel_order/1]).

create_order(OrderData) ->
    %% Pure business logic
    case validate_order(OrderData) of
        {ok, Order} ->
            %% Delegate persistence
            order_repository:save(Order);
        Error ->
            Error
    end.
```

---

## Testing Best Practices

### 1. Test Organization

```erlang
%% ✅ GOOD: Clear test structure
-module(myapp_tests).
-include_lib("eunit/include/eunit.hrl").

%%% Setup/Teardown

setup() ->
    %% Setup test environment
    {ok, Pid} = myapp:start_link(),
    Pid.

cleanup(Pid) ->
    %% Cleanup
    myapp:stop(Pid).

%%% Tests

basic_functionality_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun test_basic_operations/1}.

test_basic_operations(Pid) ->
    [
     ?_assertEqual(ok, myapp:operation1(Pid)),
     ?_assertMatch({ok, _}, myapp:operation2(Pid))
    ].
```

### 2. Property-Based Testing

```erlang
%% ✅ GOOD: Properties that should always hold
prop_encode_decode() ->
    ?FORALL(Data, binary(),
            Data =:= decode(encode(Data))).

prop_sort_idempotent() ->
    ?FORALL(List, list(integer()),
            lists:sort(List) =:= lists:sort(lists:sort(List))).
```

### 3. Test Concurrent Code

```erlang
%% ✅ GOOD: Test concurrent access
concurrent_access_test() ->
    Server = start_server(),
    
    %% Spawn multiple clients
    Pids = [spawn(fun() ->
                      Parent = self(),
                      Result = gen_server:call(Server, request),
                      Parent ! {self(), Result}
                  end) || _ <- lists:seq(1, 100)],
    
    %% Collect results
    Results = [receive {Pid, R} -> R end || Pid <- Pids],
    
    %% Verify
    ?assertEqual(100, length(Results)),
    ?assert(lists:all(fun({ok, _}) -> true end, Results)).
```

---

## Common Gotchas

### 1. Atom Table Exhaustion

```erlang
%% ❌ BAD: Creating atoms from user input
process_request(UserInput) ->
    list_to_atom(UserInput).  % Can exhaust atom table!

%% ✅ GOOD: Use existing atoms or binaries
process_request(<<"start">>) -> start;
process_request(<<"stop">>) -> stop;
process_request(Other) -> {unknown, Other}.
```

### 2. Process Leaks

```erlang
%% ❌ BAD: Spawning without cleanup
start_worker(Data) ->
    spawn(fun() -> worker_loop(Data) end).

%% ✅ GOOD: Link or monitor
start_worker(Data) ->
    spawn_link(fun() -> worker_loop(Data) end).

%% ✅ GOOD: Register and cleanup
start_worker(Name, Data) ->
    Pid = spawn(fun() -> worker_loop(Data) end),
    register(Name, Pid),
    Pid.
```

### 3. Large Messages

```erlang
%% ❌ BAD: Sending large messages
Server ! {request, LargeData}.  % Copies data

%% ✅ GOOD: Use ETS or shared binary
ets:insert(shared_data, {key, LargeData}),
Server ! {request, key}.

%% ✅ GOOD: Stream data
send_chunked(Server, LargeData) ->
    Chunks = chunk_data(LargeData, 1000),
    [Server ! {chunk, Chunk} || Chunk <- Chunks],
    Server ! done.
```

### 4. String vs Binary Confusion

```erlang
%% ⚠️ GOTCHA: Strings are lists!
"hello" =:= [104, 101, 108, 108, 111].  % true

%% Use binaries for text
<<"hello">> =:= <<104, 101, 108, 108, 111>>.  % true
```

### 5. Match vs Comparison

```erlang
%% ⚠️ GOTCHA: = is pattern match, not assignment
X = 5,
X = 6.  % Error: no match of right hand side value 6

%% Use =:= for comparison
X =:= 5.  % true
```

### 6. Float Precision

```erlang
%% ⚠️ GOTCHA: Float arithmetic
0.1 + 0.2 =:= 0.3.  % false!

%% Use epsilon for float comparison
float_equal(A, B, Epsilon) ->
    abs(A - B) < Epsilon.

float_equal(0.1 + 0.2, 0.3, 0.0001).  % true
```

---

## Quick Reference

### Process Information

```erlang
%% Get process info
process_info(Pid).
process_info(Pid, memory).
process_info(Pid, message_queue_len).

%% List all processes
processes().

%% Find process by name
whereis(registered_name).
```

### Debugging

```erlang
%% Debug output
io:format("Debug: ~p~n", [Value]).

%% Trace
dbg:tracer().
dbg:p(all, c).
dbg:tpl(module, function, []).

%% Observer (GUI)
observer:start().
```

### Shell Commands

```erlang
%% Compile and load
c(module).

%% Load module
l(module).

%% Help
h().

%% Show bindings
b().

%% Forget bindings
f().

%% Process info
i().

%% Memory
memory().
```

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial Erlang best practices guide |

---

## References

- [Erlang Programming Rules](https://www.erlang.se/doc/programming_rules.shtml)
- [Learn You Some Erlang](https://learnyousomeerlang.com/)
- [Erlang Efficiency Guide](https://www.erlang.org/doc/efficiency_guide/introduction.html)
- [OTP Design Principles](https://www.erlang.org/doc/design_principles/des_princ.html)

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

