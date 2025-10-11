# AI Integration Manual - Erlang

**Version:** 1.0.0  
**Last Updated:** 2025-10-11  
**Target Audience:** AI Agents (LLM-based development assistants)  
**Language:** Erlang/OTP 26+

---

## Table of Contents

1. [Introduction](#introduction)
2. [Erlang Environment Setup](#erlang-environment-setup)
3. [Project Structure](#project-structure)
4. [Build Tool (Rebar3)](#build-tool-rebar3)
5. [Testing Framework](#testing-framework)
6. [Code Style & Formatting](#code-style--formatting)
7. [Documentation (EDoc)](#documentation-edoc)
8. [OTP Design Principles](#otp-design-principles)
9. [Implementation Guidelines](#implementation-guidelines)
10. [Error Handling](#error-handling)
11. [Concurrency & Distribution](#concurrency--distribution)
12. [Performance Optimization](#performance-optimization)
13. [Security Best Practices](#security-best-practices)
14. [Continuous Integration](#continuous-integration)
15. [Library Publishing](#library-publishing)

---

## Introduction

This manual extends the base AI Integration Manual Template with Erlang-specific practices, tools, and workflows. It focuses on building concurrent, distributed, and fault-tolerant systems using OTP (Open Telecom Platform).

### Core Principles for Erlang

1. **Let It Crash**: Design for failure, use supervision trees
2. **Concurrency**: Lightweight processes for massive concurrency
3. **Distribution**: Built-in support for distributed systems
4. **Pattern Matching**: Fundamental for control flow and data extraction
5. **Immutability**: All data is immutable by default
6. **Hot Code Loading**: Update code without stopping the system
7. **OTP Behaviors**: Use standard patterns (gen_server, gen_statem, supervisor)

---

## Erlang Environment Setup

### Required Tools

```bash
# Install Erlang/OTP via package manager

# Ubuntu/Debian
sudo apt-get update
sudo apt-get install erlang erlang-dev erlang-tools

# macOS with Homebrew
brew install erlang

# From source (recommended for latest version)
wget https://erlang.org/download/otp_src_26.2.tar.gz
tar -xzf otp_src_26.2.tar.gz
cd otp_src_26.2
./configure
make
sudo make install

# Or use asdf (version manager)
asdf plugin add erlang
asdf install erlang 26.2
asdf global erlang 26.2

# Install Rebar3 (build tool)
wget https://s3.amazonaws.com/rebar3/rebar3
chmod +x rebar3
sudo mv rebar3 /usr/local/bin/

# Or via Homebrew
brew install rebar3

# Verify installations
erl -version
rebar3 version
```

### Version Requirements

- **Minimum Erlang/OTP**: 24.0
- **Recommended**: OTP 26.x (latest stable)
- **Rebar3**: 3.22+

### Environment Configuration

```bash
# Set Erlang environment variables
export ERL_LIBS="/usr/local/lib/erlang/lib"
export PATH="$PATH:/usr/local/bin"

# Create .erlang file for interactive shell customization
cat > ~/.erlang << 'EOF'
% Shell history
io:format("~nLoaded .erlang configuration~n").

% Helper functions
c() -> 
    io:format("~nAvailable commands:~n"),
    io:format("  c:cd(Dir) - change directory~n"),
    io:format("  c:ls() - list directory~n"),
    io:format("  c:pwd() - print working directory~n"),
    io:format("  c() - help~n"),
    ok.
EOF
```

---

## Project Structure

### Standard Erlang Project Layout

```
project-root/
├── .github/
│   └── workflows/
│       ├── ci.yml
│       └── release.yml
├── .gitignore
├── rebar.config
├── rebar.lock
├── LICENSE
├── README.md
├── CHANGELOG.md
├── doc/
│   ├── overview.edoc
│   └── specs/
├── src/
│   ├── project_name.app.src      # Application resource file
│   ├── project_name_app.erl      # Application behavior
│   ├── project_name_sup.erl      # Main supervisor
│   ├── project_name_server.erl   # Gen_server example
│   └── project_name_worker.erl   # Worker module
├── include/
│   └── project_name.hrl          # Header files
├── priv/                          # Private files (config, etc)
│   └── templates/
├── test/
│   ├── project_name_SUITE.erl    # Common Test suite
│   └── prop_project_name.erl     # PropEr property tests
├── config/
│   ├── sys.config                # Runtime configuration
│   └── vm.args                   # VM arguments
└── _build/                        # Build artifacts (gitignored)
```

### OTP Application Structure

```
src/
├── myapp.app.src              % Application specification
├── myapp_app.erl              % Application callback
├── myapp_sup.erl              % Top-level supervisor
├── myapp_server.erl           % gen_server
├── myapp_event.erl            % gen_event
├── myapp_statem.erl           % gen_statem (state machine)
└── myapp_worker.erl           % Worker process
```

---

## Build Tool (Rebar3)

### rebar.config

```erlang
%% -*- erlang -*-

{erl_opts, [
    debug_info,
    warnings_as_errors,
    warn_export_all,
    warn_unused_import,
    warn_untyped_record,
    {i, "include"}
]}.

{deps, [
    {cowboy, "2.10.0"},
    {jiffy, "1.1.1"},
    {poolboy, "1.5.2"}
]}.

{project_plugins, [
    rebar3_proper,
    rebar3_ex_doc,
    rebar3_hex,
    rebar3_lint
]}.

{shell, [
    {config, "config/sys.config"},
    {apps, [myapp]}
]}.

{relx, [
    {release, {myapp, "1.0.0"}, [myapp, sasl]},
    {dev_mode, true},
    {include_erts, false},
    
    {sys_config, "./config/sys.config"},
    {vm_args, "./config/vm.args"},
    
    {overlay, [
        {mkdir, "log"},
        {copy, "priv", "priv"}
    ]}
]}.

{profiles, [
    {prod, [
        {erl_opts, [no_debug_info, warnings_as_errors]},
        {relx, [
            {dev_mode, false},
            {include_erts, true},
            {include_src, false}
        ]}
    ]},
    {test, [
        {deps, [
            {meck, "0.9.2"},
            {proper, "1.4.0"}
        ]},
        {erl_opts, [debug_info, export_all, nowarn_export_all]}
    ]}
]}.

{cover_enabled, true}.
{cover_opts, [verbose]}.
{cover_excl_mods, [myapp_tests]}.

{dialyzer, [
    {warnings, [
        error_handling,
        race_conditions,
        underspecs,
        unknown
    ]},
    {plt_extra_apps, [ssl, crypto]}
]}.

{xref_checks, [
    undefined_function_calls,
    undefined_functions,
    locals_not_used,
    deprecated_function_calls,
    deprecated_functions
]}.

{alias, [
    {check, [xref, dialyzer, edoc, ct, cover]}
]}.
```

### Application Resource File

**src/myapp.app.src**:
```erlang
{application, myapp, [
    {description, "My Erlang Application"},
    {vsn, "1.0.0"},
    {registered, [myapp_sup]},
    {mod, {myapp_app, []}},
    {applications, [
        kernel,
        stdlib,
        sasl,
        crypto,
        ssl
    ]},
    {env, [
        {port, 8080},
        {pool_size, 10}
    ]},
    {modules, []},
    {licenses, ["MIT"]},
    {links, [{"Github", "https://github.com/user/myapp"}]}
]}.
```

### Rebar3 Commands

```bash
# Create new project
rebar3 new app myapp
rebar3 new release myapp
rebar3 new lib mylib

# Compile
rebar3 compile

# Run shell with application loaded
rebar3 shell

# Run tests
rebar3 eunit
rebar3 ct
rebar3 proper

# Coverage
rebar3 cover

# Type checking
rebar3 dialyzer

# Cross-reference analysis
rebar3 xref

# Generate documentation
rebar3 edoc

# Build release
rebar3 release

# Clean
rebar3 clean

# Check everything
rebar3 do compile, xref, dialyzer, ct, cover

# Publish to Hex
rebar3 hex publish
```

---

## Testing Framework

### EUnit Tests

**test/myapp_tests.erl**:
```erlang
-module(myapp_tests).
-include_lib("eunit/include/eunit.hrl").

%%% Test Descriptions
%%% This module tests the core functionality of myapp

%% Setup and cleanup
setup() ->
    {ok, Pid} = myapp_server:start_link(),
    Pid.

cleanup(Pid) ->
    myapp_server:stop(Pid).

%% Test using setup/cleanup
with_server_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_test(test_basic_operation(Pid)),
          ?_test(test_error_handling(Pid))
         ]
     end}.

test_basic_operation(Pid) ->
    Result = myapp_server:process(Pid, <<"test">>),
    ?assertEqual(ok, Result).

test_error_handling(Pid) ->
    Result = myapp_server:process(Pid, invalid),
    ?assertMatch({error, _}, Result).

%% Simple test
simple_test() ->
    ?assertEqual(4, 2 + 2).

%% Test with assertion
add_test() ->
    ?assert(myapp_math:add(2, 3) =:= 5).

%% Negative test
divide_by_zero_test() ->
    ?assertError(badarith, myapp_math:divide(10, 0)).

%% Test generator
numbers_test_() ->
    [?_assertEqual(N * 2, myapp_math:double(N)) 
     || N <- lists:seq(1, 10)].

%% Timeout test
slow_operation_test_() ->
    {timeout, 5, fun() ->
        Result = myapp:slow_operation(),
        ?assertEqual(ok, Result)
    end}.
```

### Common Test

**test/myapp_SUITE.erl**:
```erlang
-module(myapp_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Test suite exports
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([test_basic_functionality/1,
         test_concurrent_access/1,
         test_error_recovery/1]).

%%% Suite configuration

all() ->
    [
     {group, basic},
     {group, concurrent},
     {group, error_handling}
    ].

groups() ->
    [
     {basic, [sequence], [
         test_basic_functionality
     ]},
     {concurrent, [parallel], [
         test_concurrent_access
     ]},
     {error_handling, [], [
         test_error_recovery
     ]}
    ].

%%% Setup and teardown

init_per_suite(Config) ->
    application:ensure_all_started(myapp),
    [{app_started, true} | Config].

end_per_suite(_Config) ->
    application:stop(myapp),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%% Test cases

test_basic_functionality(Config) ->
    ct:log("Testing basic functionality"),
    {ok, Result} = myapp:process(<<"test">>),
    true = is_binary(Result),
    ok.

test_concurrent_access(Config) ->
    ct:log("Testing concurrent access"),
    Parent = self(),
    
    %% Spawn multiple processes
    Pids = [spawn(fun() ->
                      Result = myapp:process(<<"data">>),
                      Parent ! {self(), Result}
                  end) || _ <- lists:seq(1, 100)],
    
    %% Wait for all results
    Results = [receive {Pid, R} -> R end || Pid <- Pids],
    
    %% Verify all succeeded
    true = lists:all(fun({ok, _}) -> true; (_) -> false end, Results),
    ok.

test_error_recovery(Config) ->
    ct:log("Testing error recovery"),
    {error, _Reason} = myapp:process(invalid_input),
    
    %% System should still work after error
    {ok, _Result} = myapp:process(<<"valid">>),
    ok.
```

### PropEr Property Tests

**test/prop_myapp.erl**:
```erlang
-module(prop_myapp).
-include_lib("proper/include/proper.hrl").

%%% Properties

prop_add_commutative() ->
    ?FORALL({X, Y}, {integer(), integer()},
            myapp_math:add(X, Y) =:= myapp_math:add(Y, X)).

prop_list_reverse() ->
    ?FORALL(List, list(integer()),
            lists:reverse(lists:reverse(List)) =:= List).

prop_encode_decode() ->
    ?FORALL(Data, binary(),
            begin
                Encoded = myapp_codec:encode(Data),
                Decoded = myapp_codec:decode(Encoded),
                Decoded =:= Data
            end).

%% Custom generator
user_generator() ->
    ?LET({Name, Age},
         {non_empty(string()), range(0, 120)},
         #{name => Name, age => Age}).

prop_user_validation() ->
    ?FORALL(User, user_generator(),
            myapp_user:validate(User) =:= true).
```

---

## Code Style & Formatting

### Erlang Style Guide

```erlang
%%% @doc Module documentation
%%% This module implements...
%%% @end

-module(myapp_server).
-behaviour(gen_server).

%%% API
-export([start_link/0, stop/1, process/2]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% Records
-record(state, {
    counter = 0 :: integer(),
    data = #{} :: map(),
    workers = [] :: [pid()]
}).

%%% Types
-type state() :: #state{}.
-type process_result() :: {ok, term()} | {error, term()}.

%%% Macros
-define(SERVER, ?MODULE).
-define(TIMEOUT, 5000).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Stop the server
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Process data
-spec process(pid(), binary()) -> process_result().
process(Pid, Data) when is_binary(Data) ->
    gen_server:call(Pid, {process, Data}, ?TIMEOUT);
process(_Pid, _Data) ->
    {error, invalid_input}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({process, Data}, _From, State) ->
    Result = do_process(Data, State),
    NewState = State#state{counter = State#state.counter + 1},
    {reply, {ok, Result}, NewState};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_process(Data, _State) when is_binary(Data) ->
    binary:copy(Data).
```

### Formatting Tools

```bash
# Using rebar3_format plugin
# Add to rebar.config:
{project_plugins, [rebar3_format]}.

# Format code
rebar3 format

# Check formatting
rebar3 format --verify
```

---

## Documentation (EDoc)

### EDoc Comments

```erlang
%%% @doc
%%% Core server module for MyApp.
%%%
%%% This module implements a gen_server that manages application state
%%% and processes incoming requests.
%%%
%%% == Example ==
%%%
%%% ```
%%% {ok, Pid} = myapp_server:start_link(),
%%% {ok, Result} = myapp_server:process(Pid, <<"data">>),
%%% ok = myapp_server:stop(Pid).
%%% '''
%%%
%%% @author Your Name <your@email.com>
%%% @version 1.0.0
%%% @end

-module(myapp_server).

%%% @doc
%%% Start the server.
%%%
%%% Starts a new instance of the server and registers it locally.
%%%
%%% @returns `{ok, Pid}' if successful, `{error, Reason}' otherwise.
%%% @end
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% @doc
%%% Process data asynchronously.
%%%
%%% @param Pid The server process ID
%%% @param Data The data to process (must be binary)
%%% @returns `{ok, Result}' or `{error, Reason}'
%%% @throws timeout if operation takes too long
%%% @see process_sync/2
%%% @end
-spec process(pid(), binary()) -> {ok, term()} | {error, term()}.
process(Pid, Data) ->
    gen_server:call(Pid, {process, Data}).
```

### Generating Documentation

```bash
# Generate EDoc
rebar3 edoc

# Generated docs in doc/ directory
open doc/index.html

# Using ex_doc (better formatting)
rebar3 ex_doc

# Specify options in rebar.config:
{ex_doc, [
    {source_url, <<"https://github.com/user/myapp">>},
    {extras, [<<"README.md">>, <<"CHANGELOG.md">>]},
    {main, <<"readme">>}
]}.
```

---

## OTP Design Principles

### Application Behavior

**src/myapp_app.erl**:
```erlang
-module(myapp_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case myapp_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.
```

### Supervisor

**src/myapp_sup.erl**:
```erlang
-module(myapp_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 5
    },
    
    ChildSpecs = [
        #{
            id => myapp_server,
            start => {myapp_server, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [myapp_server]
        },
        #{
            id => myapp_worker_sup,
            start => {myapp_worker_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [myapp_worker_sup]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
```

### Gen_Server

**src/myapp_server.erl**:
```erlang
-module(myapp_server).
-behaviour(gen_server).

%% API
-export([start_link/0, get_state/0, set_state/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    data = #{} :: map(),
    counter = 0 :: non_neg_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_state() ->
    gen_server:call(?MODULE, get_state).

set_state(NewData) ->
    gen_server:cast(?MODULE, {set_state, NewData}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(get_state, _From, State) ->
    {reply, {ok, State#state.data}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({set_state, NewData}, State) ->
    NewState = State#state{
        data = NewData,
        counter = State#state.counter + 1
    },
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

### Gen_StateMachine

**src/myapp_statem.erl**:
```erlang
-module(myapp_statem).
-behaviour(gen_statem).

-export([start_link/0, connect/0, disconnect/0, send/1]).
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([disconnected/3, connected/3]).

-record(data, {
    socket = undefined :: undefined | port(),
    buffer = [] :: list()
}).

%%% API

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

connect() ->
    gen_statem:call(?MODULE, connect).

disconnect() ->
    gen_statem:cast(?MODULE, disconnect).

send(Message) ->
    gen_statem:call(?MODULE, {send, Message}).

%%% Callbacks

callback_mode() ->
    state_functions.

init([]) ->
    {ok, disconnected, #data{}}.

%%% State: disconnected

disconnected({call, From}, connect, Data) ->
    case do_connect() of
        {ok, Socket} ->
            NewData = Data#data{socket = Socket},
            {next_state, connected, NewData, [{reply, From, ok}]};
        Error ->
            {keep_state_and_data, [{reply, From, Error}]}
    end;
disconnected({call, From}, {send, _Message}, _Data) ->
    {keep_state_and_data, [{reply, From, {error, not_connected}}]};
disconnected(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, disconnected, Data).

%%% State: connected

connected({call, From}, {send, Message}, Data = #data{socket = Socket}) ->
    Result = gen_tcp:send(Socket, Message),
    {keep_state_and_data, [{reply, From, Result}]};
connected(cast, disconnect, Data = #data{socket = Socket}) ->
    gen_tcp:close(Socket),
    {next_state, disconnected, Data#data{socket = undefined}};
connected(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, connected, Data).

%%% Common event handler

handle_event(_EventType, _EventContent, State, Data) ->
    {next_state, State, Data}.

terminate(_Reason, _State, #data{socket = Socket}) ->
    case Socket of
        undefined -> ok;
        _ -> gen_tcp:close(Socket)
    end.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%% Internal functions

do_connect() ->
    gen_tcp:connect("localhost", 8080, [binary, {active, false}]).
```

---

## Implementation Guidelines

### Pattern Matching

```erlang
%%% Pattern matching in function heads
process_message({ok, Data}) ->
    handle_success(Data);
process_message({error, Reason}) ->
    handle_error(Reason);
process_message(timeout) ->
    handle_timeout().

%%% Pattern matching with guards
is_valid_age(Age) when is_integer(Age), Age >= 0, Age =< 120 ->
    true;
is_valid_age(_) ->
    false.

%%% Pattern matching in case expressions
parse_result(Result) ->
    case Result of
        {ok, Value} when is_integer(Value) ->
            {parsed, Value * 2};
        {ok, Value} when is_binary(Value) ->
            {parsed, binary_to_list(Value)};
        {error, _} = Error ->
            Error
    end.

%%% Destructuring records
-record(user, {name, age, email}).

greet(#user{name = Name, age = Age}) ->
    io:format("Hello ~s, you are ~p years old~n", [Name, Age]).
```

### List Comprehensions

```erlang
%%% Basic list comprehension
Doubled = [X * 2 || X <- [1, 2, 3, 4, 5]].
%% Result: [2, 4, 6, 8, 10]

%%% With filter
Evens = [X || X <- [1, 2, 3, 4, 5, 6], X rem 2 =:= 0].
%% Result: [2, 4, 6]

%%% Multiple generators
Pairs = [{X, Y} || X <- [1, 2, 3], Y <- [a, b]].
%% Result: [{1,a},{1,b},{2,a},{2,b},{3,a},{3,b}]

%%% Binary comprehensions
Bin = << <<X>> || X <- [1, 2, 3, 4, 5] >>.
%% Result: <<1,2,3,4,5>>
```

### Higher-Order Functions

```erlang
%%% Map
lists:map(fun(X) -> X * 2 end, [1, 2, 3]).
%% Result: [2, 4, 6]

%%% Filter
lists:filter(fun(X) -> X rem 2 =:= 0 end, [1, 2, 3, 4, 5]).
%% Result: [2, 4]

%%% Fold (reduce)
lists:foldl(fun(X, Acc) -> X + Acc end, 0, [1, 2, 3, 4, 5]).
%% Result: 15

%%% Custom higher-order function
apply_twice(Fun, Value) ->
    Fun(Fun(Value)).

apply_twice(fun(X) -> X * 2 end, 5).
%% Result: 20
```

---

## Error Handling

### Let It Crash Philosophy

```erlang
%%% Don't defensively check everything
%%% Let supervisor restart the process

%% ❌ BAD: Defensive programming
process_data(undefined) ->
    {error, undefined_data};
process_data(<<>>) ->
    {error, empty_data};
process_data(Data) when not is_binary(Data) ->
    {error, invalid_type};
process_data(Data) ->
    %% Finally do work
    {ok, Data}.

%% ✅ GOOD: Let it crash
process_data(Data) when is_binary(Data), byte_size(Data) > 0 ->
    %% Process will crash if Data is invalid
    %% Supervisor will restart it
    {ok, binary:copy(Data)}.
```

### Try-Catch

```erlang
%%% Try-catch for handling expected errors
safe_divide(A, B) ->
    try
        A / B
    catch
        error:badarith ->
            {error, division_by_zero};
        error:Reason ->
            {error, Reason}
    end.

%%% With after clause for cleanup
read_file(Filename) ->
    {ok, File} = file:open(Filename, [read]),
    try
        file:read(File, 1024)
    after
        file:close(File)
    end.
```

### Error Tuples

```erlang
%%% Standard error convention: {ok, Result} | {error, Reason}

lookup_user(UserId) ->
    case database:find(users, UserId) of
        {ok, User} ->
            {ok, User};
        not_found ->
            {error, user_not_found};
        {error, Reason} ->
            {error, {database_error, Reason}}
    end.

%%% Chaining operations
process_user_request(UserId) ->
    case lookup_user(UserId) of
        {ok, User} ->
            case validate_user(User) of
                ok ->
                    authorize_user(User);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.
```

---

## Concurrency & Distribution

### Spawning Processes

```erlang
%%% Simple spawn
Pid = spawn(fun() ->
    receive
        {From, Message} ->
            From ! {self(), process(Message)}
    end
end).

%%% Spawn with link (crash together)
Pid = spawn_link(fun() -> worker_loop() end).

%%% Spawn with monitor (get notified on crash)
{Pid, Ref} = spawn_monitor(fun() -> worker_loop() end).

%%% Receive monitor message
receive
    {'DOWN', Ref, process, Pid, Reason} ->
        handle_process_death(Reason)
end.
```

### Message Passing

```erlang
%%% Sending messages
Pid ! {self(), hello},
Pid ! {request, Data}.

%%% Receiving messages
receive
    {From, hello} ->
        From ! {self(), hi},
        ok;
    {request, Data} ->
        handle_request(Data);
    Other ->
        handle_unknown(Other)
after
    5000 ->
        timeout
end.

%%% Selective receive
receive
    {priority, urgent, Message} ->
        handle_urgent(Message)
after
    0 ->
        receive
            {priority, normal, Message} ->
                handle_normal(Message)
        after
            1000 ->
                timeout
        end
end.
```

### Process Pooling

```erlang
-module(myapp_pool).

-export([start_link/1, checkout/1, checkin/2]).

start_link(PoolSize) ->
    Pid = spawn_link(fun() -> pool_loop(PoolSize, []) end),
    {ok, Pid}.

checkout(Pool) ->
    Pool ! {checkout, self()},
    receive
        {worker, Worker} -> {ok, Worker};
        pool_empty -> {error, no_workers}
    after
        5000 -> {error, timeout}
    end.

checkin(Pool, Worker) ->
    Pool ! {checkin, Worker},
    ok.

pool_loop(PoolSize, Available) when length(Available) < PoolSize ->
    Worker = spawn_link(fun worker_loop/0),
    pool_loop(PoolSize, [Worker | Available]);
pool_loop(PoolSize, Available) ->
    receive
        {checkout, From} when Available =/= [] ->
            [Worker | Rest] = Available,
            From ! {worker, Worker},
            pool_loop(PoolSize, Rest);
        {checkout, From} ->
            From ! pool_empty,
            pool_loop(PoolSize, Available);
        {checkin, Worker} ->
            pool_loop(PoolSize, [Worker | Available])
    end.

worker_loop() ->
    receive
        {work, Data, From} ->
            Result = do_work(Data),
            From ! {result, Result},
            worker_loop();
        stop ->
            ok
    end.

do_work(Data) ->
    %% Simulate work
    timer:sleep(100),
    {processed, Data}.
```

### Distributed Erlang

```erlang
%%% Start distributed node
%% erl -name node1@hostname -setcookie secret

%%% Connect to another node
net_adm:ping('node2@hostname').

%%% Spawn process on remote node
Pid = spawn('node2@hostname', fun() -> worker() end).

%%% Send message to remote process
{server, 'node2@hostname'} ! {request, Data}.

%%% Register global name
global:register_name(my_server, Pid).

%%% Send to globally registered process
global:send(my_server, {request, Data}).
```

---

## Performance Optimization

### Binary Optimization

```erlang
%%% Efficient binary construction
build_response(Items) ->
    iolist_to_binary([
        <<"[">>,
        lists:join(<<",">>, [item_to_json(I) || I <- Items]),
        <<"]">>
    ]).

%%% Pattern matching binaries
parse_header(<<Type:8, Length:16, Rest/binary>>) ->
    <<Payload:Length/binary, Remaining/binary>> = Rest,
    {Type, Payload, Remaining}.

%%% Binary comprehension
encode_list(List) ->
    << <<X:32>> || X <- List >>.
```

### ETS (Erlang Term Storage)

```erlang
%%% Create ETS table
Table = ets:new(users, [set, public, named_table, {keypos, 2}]),

%%% Insert
ets:insert(users, {user, 1, <<"John">>, 30}),

%%% Lookup
case ets:lookup(users, 1) of
    [{user, 1, Name, Age}] ->
        {ok, Name, Age};
    [] ->
        not_found
end.

%%% Match patterns
ets:match(users, {user, '$1', <<"John">>, '_'}),

%%% Select with match spec
ets:select(users, [{
    {user, '$1', '$2', '$3'},
    [{'>', '$3', 25}],
    ['$_']
}]).
```

### Process Dictionary (Use Sparingly)

```erlang
%%% Store value
put(counter, 0),

%%% Retrieve value
Counter = get(counter),

%%% Update
put(counter, Counter + 1),

%%% Get all
AllValues = get(),

%%% Erase
erase(counter).

%% Note: Prefer passing state explicitly in most cases
```

---

## Security Best Practices

### Input Validation

```erlang
validate_user_input(Input) ->
    case Input of
        #{<<"username">> := Username, <<"age">> := Age}
          when is_binary(Username), byte_size(Username) > 0,
               is_integer(Age), Age >= 0, Age =< 120 ->
            {ok, #{username => Username, age => Age}};
        _ ->
            {error, invalid_input}
    end.

%%% Sanitize input
sanitize_string(String) when is_binary(String) ->
    re:replace(String, <<"[<>\"']">>, <<"">>, [global, {return, binary}]).
```

### SQL Injection Prevention

```erlang
%%% Use parameterized queries with epgsql
get_user(Conn, Email) ->
    Query = "SELECT id, name FROM users WHERE email = $1",
    case epgsql:equery(Conn, Query, [Email]) of
        {ok, _, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.
```

### Secure Communication

```erlang
%%% SSL/TLS configuration
start_ssl_server(Port, Certfile, Keyfile) ->
    SslOpts = [
        {certfile, Certfile},
        {keyfile, Keyfile},
        {versions, ['tlsv1.2', 'tlsv1.3']},
        {ciphers, [
            "ECDHE-ECDSA-AES256-GCM-SHA384",
            "ECDHE-RSA-AES256-GCM-SHA384"
        ]}
    ],
    ssl:listen(Port, [binary, {packet, 0}, {active, false} | SslOpts]).
```

---

## Continuous Integration

### GitHub Actions Workflow

**.github/workflows/ci.yml**:
```yaml
name: Erlang CI

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main, develop ]

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        otp: ['24', '25', '26']
        rebar3: ['3.22.0']

    steps:
      - uses: actions/checkout@v3

      - name: Setup Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: ${{ matrix.otp }}
          rebar3-version: ${{ matrix.rebar3 }}

      - name: Restore dependencies cache
        uses: actions/cache@v3
        with:
          path: _build
          key: ${{ runner.os }}-mix-${{ hashFiles('**/rebar.lock') }}
          restore-keys: ${{ runner.os }}-mix-

      - name: Compile
        run: rebar3 compile

      - name: Run xref
        run: rebar3 xref

      - name: Run dialyzer
        run: rebar3 dialyzer

      - name: Run tests
        run: rebar3 do eunit, ct

      - name: Generate coverage
        run: rebar3 cover

      - name: Upload coverage
        uses: codecov/codecov-action@v3
        if: matrix.otp == '26'
        with:
          files: ./eunit.coverdata

  release:
    needs: test
    if: github.ref == 'refs/heads/main'
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3

      - name: Setup Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: '26'
          rebar3-version: '3.22.0'

      - name: Build release
        run: rebar3 as prod release

      - name: Create tarball
        run: rebar3 as prod tar

      - name: Upload artifacts
        uses: actions/upload-artifact@v3
        with:
          name: release
          path: _build/prod/rel/myapp/myapp-*.tar.gz
```

---

## Library Publishing

### Publishing to Hex

```bash
# Ensure rebar3_hex plugin is installed
# In rebar.config:
{project_plugins, [rebar3_hex]}.

# Authenticate
rebar3 hex user auth

# Publish
rebar3 hex publish

# Cut a release
rebar3 hex cut
```

### Release Configuration

**relx configuration in rebar.config**:
```erlang
{relx, [
    {release, {myapp, "1.0.0"}, [myapp, sasl, runtime_tools]},
    
    {mode, prod},
    {include_erts, true},
    {extended_start_script, true},
    
    {sys_config, "./config/sys.config"},
    {vm_args, "./config/vm.args"},
    
    {overlay, [
        {mkdir, "log"},
        {copy, "priv", "priv"}
    ]}
]}.
```

---

## Quick Reference

### Essential Commands

```bash
# Start Erlang shell
erl

# Compile module
c(module_name).

# Reload module
l(module_name).

# Run function
module_name:function_name(Args).

# Shell helpers
h().           % Help
b().           % Show bindings
f().           % Forget all bindings
f(Var).        % Forget variable
c:cd(Dir).     % Change directory
c:ls().        % List directory
c:pwd().       % Print working directory
```

### Checklist for Implementation

- [ ] OTP 24+ installed
- [ ] Rebar3 configured
- [ ] Application behavior implemented
- [ ] Supervisor tree designed
- [ ] EUnit/Common Test tests written
- [ ] Tests passing (>90% coverage)
- [ ] Dialyzer types added
- [ ] EDoc documentation generated
- [ ] CHANGELOG updated
- [ ] Release configured

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial Erlang manual creation |

---

## References

- [Erlang Documentation](https://www.erlang.org/doc/)
- [Learn You Some Erlang](https://learnyousomeerlang.com/)
- [Erlang Programming Rules](https://www.erlang.se/doc/programming_rules.shtml)
- [OTP Design Principles](https://www.erlang.org/doc/design_principles/des_princ.html)
- [Rebar3 Documentation](https://rebar3.org/)

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

