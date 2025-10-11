# Elixir Best Practices

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Target Audience**: AI Agents developing Elixir projects

---

## Table of Contents

1. [Elixir Idioms](#elixir-idioms)
2. [Anti-Patterns](#anti-patterns)
3. [Performance Optimization](#performance-optimization)
4. [Pattern Matching](#pattern-matching)
5. [Concurrency and OTP](#concurrency-and-otp)
6. [Error Handling](#error-handling)
7. [Functional Programming](#functional-programming)
8. [Code Organization](#code-organization)
9. [Testing Best Practices](#testing-best-practices)
10. [Common Gotchas](#common-gotchas)

---

## Elixir Idioms

### 1. Use the Pipe Operator

```elixir
# ❌ BAD: Nested function calls
result = String.upcase(String.trim(String.reverse(input)))

# ✅ GOOD: Pipe operator (reads left to right)
result =
  input
  |> String.reverse()
  |> String.trim()
  |> String.upcase()

# ✅ GOOD: Multiple steps
result =
  data
  |> validate()
  |> transform()
  |> save()
  |> notify()
```

### 2. Pattern Matching Everywhere

```elixir
# ❌ BAD: Checking return value
result = fetch_user(id)
if elem(result, 0) == :ok do
  user = elem(result, 1)
  process(user)
end

# ✅ GOOD: Pattern matching
case fetch_user(id) do
  {:ok, user} -> process(user)
  {:error, reason} -> handle_error(reason)
end

# ✅ BETTER: with statement for multiple steps
with {:ok, user} <- fetch_user(id),
     {:ok, posts} <- fetch_posts(user.id),
     {:ok, comments} <- fetch_comments(user.id) do
  %{user: user, posts: posts, comments: comments}
end
```

### 3. Use Tagged Tuples

```elixir
# ❌ BAD: Bare values
def divide(a, b) do
  a / b  # What about division by zero?
end

# ✅ GOOD: Tagged tuples
def divide(a, b) when b != 0 do
  {:ok, a / b}
end

def divide(_, 0) do
  {:error, :division_by_zero}
end

# Usage with pattern matching
case divide(10, 2) do
  {:ok, result} -> IO.puts("Result: #{result}")
  {:error, :division_by_zero} -> IO.puts("Cannot divide by zero")
end
```

### 4. Leverage Guards

```elixir
# ✅ GOOD: Use guards for type checking
def process(x) when is_integer(x) and x > 0 do
  x * 2
end

def process(x) when is_binary(x) do
  String.upcase(x)
end

def process(x) when is_list(x) do
  Enum.count(x)
end

# Custom guard
defguard is_positive(number) when is_number(number) and number > 0

def calculate(x) when is_positive(x) do
  x * 2
end
```

### 5. Use Enum and Stream

```elixir
# ✅ GOOD: Enum for immediate evaluation
result =
  [1, 2, 3, 4, 5]
  |> Enum.map(&(&1 * 2))
  |> Enum.filter(&(&1 > 5))
  |> Enum.sum()

# ✅ BETTER: Stream for lazy evaluation (large datasets)
result =
  1..1_000_000
  |> Stream.map(&(&1 * 2))
  |> Stream.filter(&(&1 > 5))
  |> Enum.take(10)
```

---

## Anti-Patterns

### 1. Don't Fight Immutability

```elixir
# ❌ BAD: Trying to mutate (this doesn't work)
list = [1, 2, 3]
List.delete(list, 2)  # Returns new list
IO.inspect(list)  # Still [1, 2, 3]

# ✅ GOOD: Reassign or use new variable
list = [1, 2, 3]
list = List.delete(list, 2)
IO.inspect(list)  # [1, 3]

# Or better yet, use pipe
list =
  [1, 2, 3]
  |> List.delete(2)
  |> List.insert_at(0, 0)
```

### 2. Avoid Throwing Exceptions for Control Flow

```elixir
# ❌ BAD: Using exceptions for control flow
def fetch_user(id) do
  user = Repo.get(User, id)
  if user == nil do
    raise "User not found"
  end
  user
end

# ✅ GOOD: Return tagged tuples
def fetch_user(id) do
  case Repo.get(User, id) do
    nil -> {:error, :not_found}
    user -> {:ok, user}
  end
end

# ✅ ALTERNATIVE: Use ! version for exceptions
def fetch_user!(id) do
  case fetch_user(id) do
    {:ok, user} -> user
    {:error, reason} -> raise "User not found: #{reason}"
  end
end
```

### 3. Don't Use Processes for Everything

```elixir
# ❌ BAD: Unnecessary process
defmodule Calculator do
  use GenServer
  
  def add(a, b) do
    GenServer.call(__MODULE__, {:add, a, b})
  end
  
  def handle_call({:add, a, b}, _from, state) do
    {:reply, a + b, state}
  end
end

# ✅ GOOD: Simple function
defmodule Calculator do
  def add(a, b), do: a + b
end

# Use processes only for:
# - State management
# - Concurrent work
# - Long-running tasks
# - Isolation/fault tolerance
```

### 4. Avoid Large GenServer State

```elixir
# ❌ BAD: Storing large data in GenServer state
defmodule Cache do
  use GenServer
  
  def init(_) do
    {:ok, %{data: %{}}}  # Gets huge over time
  end
  
  def handle_call({:store, key, value}, _from, state) do
    new_data = Map.put(state.data, key, value)
    {:reply, :ok, %{state | data: new_data}}
  end
end

# ✅ GOOD: Use ETS for large data
defmodule Cache do
  use GenServer
  
  def init(_) do
    table = :ets.new(:cache, [:set, :public, :named_table])
    {:ok, %{table: table}}
  end
  
  def handle_call({:store, key, value}, _from, state) do
    :ets.insert(:cache, {key, value})
    {:reply, :ok, state}
  end
end
```

### 5. Don't Pattern Match on Large Structures

```elixir
# ❌ SLOW: Pattern matching entire struct
def process(%User{id: id, name: name, email: email, ...}) do
  # Only need id and name
end

# ✅ FAST: Match only what you need
def process(%User{id: id, name: name}) do
  # Cleaner and faster
end

# ✅ GOOD: Use map access for single field
def process(%User{} = user) do
  user.id
end
```

---

## Performance Optimization

### 1. Use Tail Recursion

```elixir
# ❌ SLOW: Not tail recursive (stack overflow for large n)
def factorial(0), do: 1
def factorial(n), do: n * factorial(n - 1)

# ✅ FAST: Tail recursive with accumulator
def factorial(n), do: factorial(n, 1)

defp factorial(0, acc), do: acc
defp factorial(n, acc), do: factorial(n - 1, n * acc)
```

### 2. Use ETS for In-Memory Storage

```elixir
# ✅ GOOD: Fast in-memory storage
def create_cache do
  :ets.new(:my_cache, [
    :set,           # Type: set, bag, or duplicate_bag
    :public,        # Access: public, protected, private
    :named_table,   # Can reference by name
    {:read_concurrency, true},
    {:write_concurrency, true}
  ])
end

def cache_put(key, value) do
  :ets.insert(:my_cache, {key, value})
end

def cache_get(key) do
  case :ets.lookup(:my_cache, key) do
    [{^key, value}] -> {:ok, value}
    [] -> {:error, :not_found}
  end
end
```

### 3. Use Binary Pattern Matching for Parsing

```elixir
# ✅ FAST: Binary pattern matching
def parse_ip(<<a, b, c, d>>) do
  "#{a}.#{b}.#{c}.#{d}"
end

def parse_header(<<
  type::8,
  length::16,
  data::binary-size(length),
  rest::binary
>>) do
  %{type: type, length: length, data: data, rest: rest}
end
```

### 4. Avoid Creating Many Small Processes

```elixir
# ❌ SLOW: Creating process per item
def process_items(items) do
  items
  |> Enum.map(fn item ->
    Task.async(fn -> process(item) end)
  end)
  |> Enum.map(&Task.await/1)
end

# ✅ FAST: Use Task.async_stream with concurrency limit
def process_items(items) do
  items
  |> Task.async_stream(&process/1, max_concurrency: System.schedulers_online() * 2)
  |> Enum.map(fn {:ok, result} -> result end)
end
```

### 5. Profile Before Optimizing

```elixir
# Profile with :fprof
:fprof.apply(&MyModule.slow_function/1, [arg])
:fprof.profile()
:fprof.analyse()

# Or use mix profile
mix profile.fprof -e "MyModule.slow_function(arg)"

# Benchmark with Benchee
Benchee.run(%{
  "version 1" => fn -> version1(data) end,
  "version 2" => fn -> version2(data) end
})
```

---

## Pattern Matching

### 1. Match Function Arguments

```elixir
# ✅ GOOD: Pattern match in function head
def greet(%User{name: name, role: :admin}) do
  "Hello Admin #{name}!"
end

def greet(%User{name: name}) do
  "Hello #{name}"
end

def greet(name) when is_binary(name) do
  "Hello #{name}"
end
```

### 2. Use Pin Operator

```elixir
# Match against existing variable value
expected = 42

case some_function() do
  ^expected -> :match
  other -> {:no_match, other}
end

# Useful in lists
x = 1
[^x, 2, 3] = [1, 2, 3]  # OK
[^x, 2, 3] = [2, 2, 3]  # MatchError
```

### 3. Destructure in Function Heads

```elixir
# ✅ GOOD: Destructure complex data
def process_response({:ok, %{status: 200, body: body}}) do
  {:ok, Jason.decode!(body)}
end

def process_response({:ok, %{status: status}}) when status >= 400 do
  {:error, :http_error}
end

def process_response({:error, reason}) do
  {:error, reason}
end
```

### 4. Match Maps and Structs

```elixir
# ✅ GOOD: Match specific fields
def process(%{type: "user", id: id, data: data}) do
  # Process user
end

def process(%{type: "post", id: id}) do
  # Process post
end

# Struct matching
def validate(%User{email: email}) when is_binary(email) do
  # Validate user
end
```

---

## Concurrency and OTP

### 1. Use Supervision Trees

```elixir
# ✅ GOOD: Proper supervision hierarchy
defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    children = [
      # Database connection pool
      {MyApp.Repo, []},
      
      # Supervisor for workers
      {MyApp.WorkerSupervisor, []},
      
      # Web endpoint
      {MyApp.Endpoint, []},
      
      # Background job processor
      {MyApp.JobQueue, []}
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
```

### 2. Handle Process Exits

```elixir
# ✅ GOOD: Link processes that should die together
def start_linked_worker do
  spawn_link(fn -> do_work() end)
end

# ✅ GOOD: Monitor processes to handle crashes
def start_monitored_worker do
  {pid, ref} = spawn_monitor(fn -> do_work() end)
  
  receive do
    {:DOWN, ^ref, :process, ^pid, reason} ->
      handle_crash(reason)
  end
end

# ✅ GOOD: Trap exits in GenServer
def init(state) do
  Process.flag(:trap_exit, true)
  {:ok, state}
end

def handle_info({:EXIT, pid, reason}, state) do
  # Handle worker exit
  {:noreply, state}
end
```

### 3. Use Task for Concurrent Work

```elixir
# ✅ GOOD: Parallel execution
tasks = [
  Task.async(fn -> fetch_user(1) end),
  Task.async(fn -> fetch_posts(1) end),
  Task.async(fn -> fetch_comments(1) end)
]

results = Task.await_many(tasks)

# ✅ GOOD: Task.async_stream for collections
items
|> Task.async_stream(&process_item/1, max_concurrency: 10)
|> Enum.to_list()
```

### 4. Implement OTP Behaviours Correctly

```elixir
# ✅ GOOD: Complete GenServer implementation
defmodule MyWorker do
  use GenServer
  
  # Required callbacks
  @impl true
  def init(args) do
    {:ok, initial_state(args)}
  end
  
  @impl true
  def handle_call(request, from, state) do
    # Handle synchronous calls
    {:reply, response, state}
  end
  
  @impl true
  def handle_cast(request, state) do
    # Handle asynchronous calls
    {:noreply, state}
  end
  
  @impl true
  def handle_info(msg, state) do
    # Handle other messages
    {:noreply, state}
  end
  
  @impl true
  def terminate(reason, state) do
    # Cleanup
    :ok
  end
end
```

---

## Error Handling

### 1. Use with for Multi-Step Operations

```elixir
# ❌ BAD: Nested case statements
def create_user(params) do
  case validate(params) do
    {:ok, valid_params} ->
      case check_uniqueness(valid_params.email) do
        :ok ->
          case insert_user(valid_params) do
            {:ok, user} ->
              {:ok, user}
            error ->
              error
          end
        error ->
          error
      end
    error ->
      error
  end
end

# ✅ GOOD: with statement
def create_user(params) do
  with {:ok, valid_params} <- validate(params),
       :ok <- check_uniqueness(valid_params.email),
       {:ok, user} <- insert_user(valid_params) do
    {:ok, user}
  end
end

# ✅ BETTER: with + else clause
def create_user(params) do
  with {:ok, valid_params} <- validate(params),
       :ok <- check_uniqueness(valid_params.email),
       {:ok, user} <- insert_user(valid_params) do
    {:ok, user}
  else
    {:error, :validation_failed} = error ->
      Logger.error("Validation failed: #{inspect(params)}")
      error
    
    {:error, :duplicate_email} = error ->
      Logger.warn("Duplicate email attempt: #{params.email}")
      error
    
    error ->
      Logger.error("Unexpected error: #{inspect(error)}")
      {:error, :unknown}
  end
end
```

### 2. Use ! Functions Appropriately

```elixir
# ✅ GOOD: Provide both versions
def fetch_user(id) do
  case Repo.get(User, id) do
    nil -> {:error, :not_found}
    user -> {:ok, user}
  end
end

def fetch_user!(id) do
  case fetch_user(id) do
    {:ok, user} -> user
    {:error, reason} -> raise "User not found: #{reason}"
  end
end

# Usage
# When you want to handle errors
case fetch_user(id) do
  {:ok, user} -> process(user)
  {:error, _} -> handle_error()
end

# When error is unexpected/programmer error
user = fetch_user!(id)
process(user)
```

### 3. Let It Crash (with Supervision)

```elixir
# ✅ GOOD: Don't try to handle every error
defmodule Worker do
  use GenServer
  
  def handle_call(:risky_operation, _from, state) do
    # Just do the operation
    # If it crashes, supervisor will restart
    result = do_risky_thing()
    {:reply, result, state}
  end
  
  # Don't catch errors unless you can actually handle them
  # def handle_call(:risky_operation, _from, state) do
  #   try do
  #     result = do_risky_thing()
  #     {:reply, result, state}
  #   catch
  #     _ -> {:reply, :error, state}  # BAD: Lost error info
  #   end
  # end
end
```

---

## Functional Programming

### 1. Prefer Pure Functions

```elixir
# ❌ BAD: Side effects hidden
def process_order(order) do
  # Side effect: sends email
  send_confirmation_email(order.customer_email)
  # Side effect: logs
  Logger.info("Order processed: #{order.id}")
  order
end

# ✅ GOOD: Pure function, side effects explicit
def process_order(order) do
  %{order | status: :processed, processed_at: DateTime.utc_now()}
end

# Caller handles side effects
def handle_order(order) do
  processed = process_order(order)
  send_confirmation_email(processed.customer_email)
  Logger.info("Order processed: #{processed.id}")
  processed
end
```

### 2. Use Higher-Order Functions

```elixir
# ✅ GOOD: Reusable transformations
def transform_list(list, fun) do
  Enum.map(list, fun)
end

# ✅ GOOD: Function composition
def pipeline do
  fn data ->
    data
    |> validate()
    |> transform()
    |> save()
  end
end

# ✅ GOOD: Partial application
def add(a), do: fn b -> a + b end

add_five = add(5)
add_five.(10)  # 15
```

### 3. Leverage Enum and Stream

```elixir
# ✅ GOOD: Enum for small collections
[1, 2, 3, 4, 5]
|> Enum.map(&(&1 * 2))
|> Enum.filter(&(&1 > 5))
|> Enum.sum()

# ✅ GOOD: Stream for large collections
File.stream!("large_file.txt")
|> Stream.map(&String.trim/1)
|> Stream.filter(&(&1 != ""))
|> Enum.take(100)
```

### 4. Use Comprehensions

```elixir
# ✅ GOOD: For comprehensions
for x <- 1..10, y <- 1..10, x * y > 50 do
  {x, y, x * y}
end

# ✅ GOOD: Into collections
for x <- 1..10, into: %{} do
  {x, x * x}
end

# ✅ GOOD: Multiple generators
for x <- ["a", "b"], y <- [1, 2, 3] do
  {x, y}
end
```

---

## Code Organization

### 1. Module Organization

```elixir
defmodule MyApp.User do
  @moduledoc """
  User context module.
  """

  # Module attributes
  @derive {Jason.Encoder, only: [:id, :name, :email]}
  
  # Type definitions
  @type t :: %__MODULE__{
    id: integer(),
    name: String.t(),
    email: String.t()
  }
  
  # Struct definition
  defstruct [:id, :name, :email]
  
  # Public API
  def create(attrs) do
    # ...
  end
  
  def update(user, attrs) do
    # ...
  end
  
  # Private functions
  defp validate(attrs) do
    # ...
  end
end
```

### 2. Context Modules

```elixir
# ✅ GOOD: Use contexts to group related functionality
defmodule MyApp.Accounts do
  @moduledoc """
  The Accounts context.
  """
  
  alias MyApp.Accounts.User
  alias MyApp.Repo
  
  def list_users do
    Repo.all(User)
  end
  
  def get_user!(id) do
    Repo.get!(User, id)
  end
  
  def create_user(attrs) do
    %User{}
    |> User.changeset(attrs)
    |> Repo.insert()
  end
end
```

### 3. Use Protocols for Polymorphism

```elixir
# Define protocol
defprotocol Renderable do
  @doc "Renders the data structure"
  def render(data)
end

# Implement for different types
defimpl Renderable, for: User do
  def render(%User{name: name}) do
    "<div>User: #{name}</div>"
  end
end

defimpl Renderable, for: Post do
  def render(%Post{title: title}) do
    "<article>#{title}</article>"
  end
end

# Usage
Renderable.render(user)
Renderable.render(post)
```

---

## Testing Best Practices

### 1. Use Descriptive Test Names

```elixir
# ✅ GOOD: Descriptive test names
describe "create_user/1" do
  test "creates user with valid attributes" do
    # ...
  end
  
  test "returns error with invalid email" do
    # ...
  end
  
  test "returns error when email already exists" do
    # ...
  end
end
```

### 2. Use Setup and Context

```elixir
# ✅ GOOD: Setup for common data
setup do
  user = insert(:user)
  %{user: user}
end

test "updates user", %{user: user} do
  assert {:ok, updated} = Accounts.update_user(user, %{name: "New Name"})
  assert updated.name == "New Name"
end
```

### 3. Test Concurrent Code

```elixir
# ✅ GOOD: Test race conditions
test "handles concurrent updates" do
  user = insert(:user, balance: 100)
  
  tasks =
    for _ <- 1..10 do
      Task.async(fn ->
        Accounts.decrement_balance(user.id, 10)
      end)
    end
  
  Task.await_many(tasks)
  
  updated = Accounts.get_user!(user.id)
  assert updated.balance == 0
end
```

---

## Common Gotchas

### 1. Atoms Are Not Garbage Collected

```elixir
# ❌ DANGEROUS: Creating atoms dynamically
def bad_function(user_input) do
  String.to_atom(user_input)  # Can exhaust atom table!
end

# ✅ SAFE: Use existing atoms or strings
def good_function(user_input) do
  String.to_existing_atom(user_input)  # Raises if atom doesn't exist
end

# Or use strings as keys
%{"key" => "value"}
```

### 2. Large Binaries in Messages

```elixir
# ❌ SLOW: Sending large binaries between processes
GenServer.cast(pid, {:process, large_binary})  # Copies entire binary

# ✅ FAST: Use references (over 64 bytes)
# Binaries > 64 bytes are reference counted
GenServer.cast(pid, {:process, large_binary})  # Just copies reference
```

### 3. List Concatenation

```elixir
# ❌ SLOW: O(n) for each concatenation
list = []
list = list ++ [1]  # O(1)
list = list ++ [2]  # O(2)
list = list ++ [3]  # O(3)

# ✅ FAST: Prepend then reverse
list = []
list = [1 | list]
list = [2 | list]
list = [3 | list]
list = Enum.reverse(list)  # O(n) once
```

### 4. Map Access with Atoms vs Strings

```elixir
# Different keys!
map = %{"name" => "John", age: 30}

map["name"]  # "John"
map.name     # Error!

map[:age]    # 30
map["age"]   # nil
```

### 5. Variable Scope in Case/If

```elixir
# ❌ ERROR: Variable not in scope
if condition do
  result = calculate()
end
use(result)  # Error: result might not be defined

# ✅ CORRECT: Assign outside
result =
  if condition do
    calculate()
  else
    default_value()
  end
use(result)
```

---

## Additional Resources

- [Elixir Style Guide](https://github.com/christopheradams/elixir_style_guide)
- [Credo (Static Analysis)](https://github.com/rrrene/credo)
- [Dialyxir (Type Checking)](https://github.com/jeremyjh/dialyxir)
- [Phoenix Framework Guides](https://hexdocs.pm/phoenix/overview.html)
- [Elixir Forum](https://elixirforum.com/)

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

