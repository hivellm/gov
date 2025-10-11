# Ruby Best Practices

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Target Audience**: AI Agents developing Ruby projects  
**Ruby Version**: 3.0+

---

## Table of Contents

1. [Ruby Idioms](#ruby-idioms)
2. [Anti-Patterns](#anti-patterns)
3. [Performance Optimization](#performance-optimization)
4. [Security Best Practices](#security-best-practices)
5. [Error Handling](#error-handling)
6. [Object-Oriented Design](#object-oriented-design)
7. [Functional Programming](#functional-programming)
8. [Code Organization](#code-organization)
9. [Testing Best Practices](#testing-best-practices)
10. [Common Gotchas](#common-gotchas)

---

## Ruby Idioms

### 1. Use Symbols for Keys

```ruby
# ❌ BAD: String keys
user = { 'name' => 'John', 'age' => 30 }

# ✅ GOOD: Symbol keys
user = { name: 'John', age: 30 }

# ✅ GOOD: Access with symbols
user[:name]
```

### 2. Use Safe Navigation Operator

```ruby
# ❌ BAD: Nested nil checks
if user && user.profile && user.profile.address
  puts user.profile.address.city
end

# ✅ GOOD: Safe navigation
puts user&.profile&.address&.city
```

### 3. Use Trailing Conditionals

```ruby
# ❌ BAD: Verbose if statement
if user.nil?
  return
end

# ✅ GOOD: Trailing conditional
return if user.nil?

# ✅ GOOD: Trailing unless
process_user unless user.invalid?

# ✅ GOOD: Modifier form
puts 'Valid user' if user.valid?
```

### 4. Use ||= for Memoization

```ruby
# ❌ BAD: Explicit memoization
def expensive_calculation
  if @result
    @result
  else
    @result = complex_operation
  end
end

# ✅ GOOD: ||= operator
def expensive_calculation
  @result ||= complex_operation
end

# ⚠️ CAREFUL: Doesn't work for false/nil
def cached_value
  # This won't cache false or nil!
  @value ||= fetch_value
end

# ✅ BETTER: Explicit check for false/nil
def cached_value
  return @value if defined?(@value)
  @value = fetch_value
end
```

### 5. Use fetch with Default Values

```ruby
# ❌ BAD: || with hash access
config = { timeout: 0 }
timeout = config[:timeout] || 30  # Returns 30, but timeout is 0!

# ✅ GOOD: fetch with default
timeout = config.fetch(:timeout, 30)  # Returns 0

# ✅ GOOD: fetch with block
timeout = config.fetch(:timeout) { calculate_default }
```

### 6. Use tap for Object Configuration

```ruby
# ❌ BAD: Repetitive variable references
user = User.new
user.name = 'John'
user.email = 'john@example.com'
user.age = 30
user

# ✅ GOOD: tap
User.new.tap do |user|
  user.name = 'John'
  user.email = 'john@example.com'
  user.age = 30
end
```

### 7. Use then (alias: yield_self) for Chaining

```ruby
# ❌ BAD: Intermediate variables
result = calculate_value(input)
result = transform(result)
result = format(result)

# ✅ GOOD: then chaining
input
  .then { |v| calculate_value(v) }
  .then { |v| transform(v) }
  .then { |v| format(v) }
```

### 8. Use Symbol#to_proc

```ruby
# ❌ BAD: Explicit block
names = users.map { |user| user.name }

# ✅ GOOD: Symbol to proc
names = users.map(&:name)

# ✅ GOOD: With arguments
numbers = [1, 2, 3, 4, 5]
doubled = numbers.map { |n| n * 2 }

# Can't use &: with arguments, but can use partial application
doubled = numbers.map { _1 * 2 }  # Ruby 2.7+ numbered parameters
```

### 9. Use Splat and Double-Splat Operators

```ruby
# Splat for arrays
def sum(*numbers)
  numbers.reduce(:+)
end

sum(1, 2, 3)  # => 6

# Double-splat for keyword arguments
def create_user(**attributes)
  User.create(**attributes)
end

create_user(name: 'John', email: 'john@example.com')

# Splat for destructuring
first, *rest, last = [1, 2, 3, 4, 5]
# first => 1, rest => [2, 3, 4], last => 5
```

### 10. Use Regex Match Operator

```ruby
# ❌ BAD: Explicit match call
if /\d+/.match(input)
  puts 'Contains numbers'
end

# ✅ GOOD: Match operator
if input =~ /\d+/
  puts 'Contains numbers'
end

# ✅ GOOD: Match with capture
if (match = input.match(/(\d+)/))
  puts "Found number: #{match[1]}"
end
```

---

## Anti-Patterns

### 1. Using for Loops

```ruby
# ❌ BAD: for loop
for item in collection
  process(item)
end

# ✅ GOOD: each
collection.each do |item|
  process(item)
end

# ✅ GOOD: Functional style
collection.each(&:process)
```

### 2. Mutating Arguments

```ruby
# ❌ BAD: Mutating input
def add_suffix(string)
  string << '_suffix'  # Mutates original!
end

name = 'John'
add_suffix(name)
puts name  # => 'John_suffix'

# ✅ GOOD: Return new value
def add_suffix(string)
  "#{string}_suffix"
end

name = 'John'
result = add_suffix(name)
puts name    # => 'John' (unchanged)
puts result  # => 'John_suffix'
```

### 3. Using and/or Instead of &&/||

```ruby
# ❌ BAD: and/or have low precedence
result = method_call and do_something  # Unexpected behavior!

# ✅ GOOD: &&/|| have correct precedence
result = method_call && do_something

# ⚠️ EXCEPTION: and/or okay for control flow
save_record or raise 'Save failed'
validate_input and process_data
```

### 4. Rescue Without Specific Exception

```ruby
# ❌ BAD: Rescuing everything
begin
  risky_operation
rescue
  # Catches even SystemExit and NoMemoryError!
end

# ✅ GOOD: Specific exception
begin
  risky_operation
rescue StandardError => e
  handle_error(e)
end

# ✅ GOOD: Multiple specific exceptions
begin
  risky_operation
rescue NetworkError, TimeoutError => e
  handle_network_error(e)
rescue ValidationError => e
  handle_validation_error(e)
end
```

### 5. Monkey Patching Core Classes

```ruby
# ❌ BAD: Modifying core classes
class String
  def shout
    upcase + '!!!'
  end
end

# ✅ GOOD: Use refinements (scoped)
module StringExtensions
  refine String do
    def shout
      upcase + '!!!'
    end
  end
end

class MyClass
  using StringExtensions
  
  def process
    'hello'.shout  # Works here
  end
end

# ✅ BETTER: Helper module
module StringHelpers
  def self.shout(string)
    string.upcase + '!!!'
  end
end

StringHelpers.shout('hello')
```

### 6. God Classes

```ruby
# ❌ BAD: Class does everything
class UserManager
  def create_user(attrs)
    # validation
    # database operations
    # email sending
    # logging
    # caching
    # 500 more lines...
  end
end

# ✅ GOOD: Single Responsibility
class UserCreator
  def initialize(validator:, mailer:, logger:)
    @validator = validator
    @mailer = mailer
    @logger = logger
  end
  
  def call(attrs)
    @validator.validate!(attrs)
    user = User.create!(attrs)
    @mailer.send_welcome(user)
    @logger.info("User created: #{user.id}")
    user
  end
end
```

### 7. Not Using Blocks for Resource Management

```ruby
# ❌ BAD: Manual resource management
file = File.open('data.txt', 'r')
content = file.read
file.close  # Easy to forget!

# ✅ GOOD: Block ensures cleanup
File.open('data.txt', 'r') do |file|
  content = file.read
end  # Automatically closed
```

### 8. Returning Different Types

```ruby
# ❌ BAD: Inconsistent return types
def find_user(id)
  user = User.find_by(id: id)
  return nil unless user
  return user if user.active?
  return false  # WTF? nil, User, or false?
end

# ✅ GOOD: Consistent return type
def find_user(id)
  user = User.find_by(id: id)
  return nil unless user
  return nil unless user.active?
  user
end

# ✅ BETTER: Explicit optional pattern
def find_active_user(id)
  User.find_by(id: id, active: true)  # Returns User or nil
end
```

---

## Performance Optimization

### 1. Use Lazy Enumerators

```ruby
# ❌ BAD: Eager evaluation creates intermediate arrays
result = (1..1_000_000)
  .select { |n| n.even? }  # Creates array of 500k elements
  .map { |n| n * 2 }       # Creates another array
  .take(10)                # Finally takes 10

# ✅ GOOD: Lazy evaluation
result = (1..1_000_000)
  .lazy
  .select { |n| n.even? }
  .map { |n| n * 2 }
  .take(10)
  .to_a  # Only processes what's needed
```

### 2. Avoid Creating Unnecessary Objects

```ruby
# ❌ BAD: String concatenation in loop
result = ''
1000.times do
  result += 'a'  # Creates 1000 new strings!
end

# ✅ GOOD: Use append or join
result = []
1000.times do
  result << 'a'
end
result.join

# ✅ BETTER: Use times with map
result = 1000.times.map { 'a' }.join
```

### 3. Use Symbols Instead of Strings

```ruby
# ❌ BAD: String keys
hash = {}
10_000.times do |i|
  hash["key_#{i}"] = i  # Creates 10k string objects
end

# ✅ GOOD: Symbol keys (interned)
hash = {}
10_000.times do |i|
  hash[:"key_#{i}"] = i  # Reuses symbol objects
end

# Note: Symbols are not garbage collected (until Ruby 2.2+)
# Use strings for dynamic keys, symbols for static keys
```

### 4. Benchmark Your Code

```ruby
require 'benchmark'

# Simple benchmark
time = Benchmark.realtime do
  expensive_operation
end
puts "Took #{time} seconds"

# Compare implementations
Benchmark.bm(20) do |x|
  x.report('Implementation A:') { implementation_a }
  x.report('Implementation B:') { implementation_b }
end

# Memory profiling
require 'memory_profiler'

report = MemoryProfiler.report do
  memory_intensive_operation
end

puts report.pretty_print
```

### 5. Use Bulk Database Operations

```ruby
# ❌ BAD: N+1 queries
users.each do |user|
  user.posts.each do |post|
    puts post.title
  end
end

# ✅ GOOD: Eager loading
users.includes(:posts).each do |user|
  user.posts.each do |post|
    puts post.title
  end
end

# ❌ BAD: Individual inserts
users.each do |user|
  user.save  # 1000 SQL inserts
end

# ✅ GOOD: Bulk insert
User.import(users)  # 1 SQL insert
```

### 6. Cache Expensive Computations

```ruby
# ❌ BAD: Recalculate every time
class Report
  def total
    @items.sum(&:amount)  # Sums every time
  end
end

# ✅ GOOD: Memoization
class Report
  def total
    @total ||= @items.sum(&:amount)
  end
  
  def add_item(item)
    @items << item
    @total = nil  # Clear cache when data changes
  end
end

# ✅ GOOD: Rails caching
class Report
  def total
    Rails.cache.fetch(['report', id, 'total'], expires_in: 1.hour) do
      @items.sum(&:amount)
    end
  end
end
```

---

## Security Best Practices

### 1. Always Use Parameterized Queries

```ruby
# ❌ BAD: SQL injection vulnerability
User.where("email = '#{params[:email]}'")

# ✅ GOOD: Parameterized query
User.where('email = ?', params[:email])

# ✅ BETTER: Hash conditions
User.where(email: params[:email])
```

### 2. Use Strong Parameters (Rails)

```ruby
# ❌ BAD: Mass assignment vulnerability
def create
  @user = User.create(params[:user])  # Dangerous!
end

# ✅ GOOD: Strong parameters
def create
  @user = User.create(user_params)
end

private

def user_params
  params.require(:user).permit(:name, :email, :age)
end

# ✅ GOOD: Nested parameters
def user_params
  params.require(:user).permit(
    :name,
    :email,
    address_attributes: [:street, :city, :zip]
  )
end
```

### 3. Sanitize HTML Output

```ruby
# ❌ BAD: XSS vulnerability
def show
  @comment = "<script>alert('XSS')</script>"
  # In view: <%= @comment.html_safe %>  # Dangerous!
end

# ✅ GOOD: Automatic escaping
# In view: <%= @comment %>  # Escaped automatically

# ✅ GOOD: Explicit sanitization
require 'sanitize'

def safe_comment
  Sanitize.fragment(user_comment, Sanitize::Config::RELAXED)
end
```

### 4. Use Secure Password Hashing

```ruby
# ❌ BAD: Plain text or weak hashing
user.password = params[:password]  # Never store plain text!
user.password = Digest::MD5.hexdigest(params[:password])  # MD5 is broken!

# ✅ GOOD: BCrypt (Rails has_secure_password)
class User < ApplicationRecord
  has_secure_password
end

user = User.new(
  email: 'user@example.com',
  password: 'secure_password',
  password_confirmation: 'secure_password'
)

user.authenticate('secure_password')  # Returns user if correct
```

### 5. Protect Against CSRF

```ruby
# Rails protects automatically with:
class ApplicationController < ActionController::Base
  protect_from_forgery with: :exception
end

# In views, forms automatically include CSRF token:
<%= form_with model: @user do |f| %>
  <%# CSRF token included automatically %>
<% end %>
```

### 6. Use Environment Variables for Secrets

```ruby
# ❌ BAD: Hard-coded secrets
API_KEY = 'sk-1234567890abcdef'  # Never do this!

# ✅ GOOD: Environment variables
API_KEY = ENV.fetch('API_KEY') { raise 'API_KEY not set' }

# ✅ GOOD: Rails credentials (encrypted)
api_key = Rails.application.credentials.api_key

# ✅ GOOD: dotenv gem for development
# .env file (not committed to git)
API_KEY=your_development_key
DATABASE_URL=postgres://localhost/mydb
```

### 7. Validate File Uploads

```ruby
# ❌ BAD: No validation
def upload
  File.write(params[:filename], params[:file].read)  # Dangerous!
end

# ✅ GOOD: Validate file type and size
def upload
  file = params[:file]
  
  # Check file size
  raise 'File too large' if file.size > 5.megabytes
  
  # Check content type
  allowed_types = %w[image/jpeg image/png image/gif]
  raise 'Invalid file type' unless allowed_types.include?(file.content_type)
  
  # Generate safe filename
  filename = SecureRandom.uuid + File.extname(file.original_filename)
  
  File.write(Rails.root.join('uploads', filename), file.read)
end
```

---

## Error Handling

### 1. Create Custom Exception Hierarchy

```ruby
# ✅ GOOD: Structured exceptions
module MyApp
  class Error < StandardError; end
  
  class ValidationError < Error
    attr_reader :field
    
    def initialize(field, message)
      @field = field
      super("#{field}: #{message}")
    end
  end
  
  class NotFoundError < Error; end
  class UnauthorizedError < Error; end
  class ConfigurationError < Error; end
end

# Usage
raise MyApp::ValidationError.new(:email, 'must be present')
```

### 2. Use Specific Exception Classes

```ruby
# ❌ BAD: Generic exceptions
raise 'User not found'  # RuntimeError

# ✅ GOOD: Specific exception
raise NotFoundError, 'User not found'

# ✅ GOOD: Custom exception with data
class NotFoundError < StandardError
  attr_reader :resource_type, :resource_id
  
  def initialize(resource_type, resource_id)
    @resource_type = resource_type
    @resource_id = resource_id
    super("#{resource_type} with id #{resource_id} not found")
  end
end

raise NotFoundError.new('User', 42)
```

### 3. Rescue Specific Exceptions

```ruby
# ❌ BAD: Rescue everything
begin
  risky_operation
rescue => e
  # Catches too much!
end

# ✅ GOOD: Specific exceptions
begin
  risky_operation
rescue ActiveRecord::RecordNotFound => e
  handle_not_found(e)
rescue ValidationError => e
  handle_validation_error(e)
rescue StandardError => e
  logger.error("Unexpected error: #{e.message}")
  raise
end
```

### 4. Use ensure for Cleanup

```ruby
# ✅ GOOD: Ensure cleanup happens
def process_file(filename)
  file = File.open(filename)
  process(file)
rescue IOError => e
  logger.error("File processing failed: #{e.message}")
ensure
  file&.close  # Always executed
end
```

### 5. Fail Fast

```ruby
# ❌ BAD: Silent failures
def create_user(attrs)
  user = User.new(attrs)
  user.save
  user  # Returns invalid user!
end

# ✅ GOOD: Fail fast
def create_user(attrs)
  User.create!(attrs)  # Raises on validation failure
end

# ✅ GOOD: Early returns
def process(user)
  return if user.nil?
  return unless user.valid?
  return if user.banned?
  
  # Main logic here
end
```

---

## Object-Oriented Design

### 1. Single Responsibility Principle

```ruby
# ❌ BAD: Class does too much
class User
  def save
    validate
    write_to_database
    send_welcome_email
    log_creation
    update_cache
    trigger_webhooks
  end
end

# ✅ GOOD: Separate responsibilities
class UserCreator
  def initialize(user, mailer:, logger:, cache:, webhook:)
    @user = user
    @mailer = mailer
    @logger = logger
    @cache = cache
    @webhook = webhook
  end
  
  def call
    @user.save!
    @mailer.send_welcome(@user)
    @logger.info("User created: #{@user.id}")
    @cache.update(@user)
    @webhook.trigger(:user_created, @user)
    @user
  end
end
```

### 2. Dependency Injection

```ruby
# ❌ BAD: Hard-coded dependencies
class ReportGenerator
  def generate
    data = Database.query('SELECT * FROM reports')
    email = Mailer.new.send(data)
  end
end

# ✅ GOOD: Injected dependencies
class ReportGenerator
  def initialize(database:, mailer:)
    @database = database
    @mailer = mailer
  end
  
  def generate
    data = @database.query('SELECT * FROM reports')
    @mailer.send(data)
  end
end

# Usage
generator = ReportGenerator.new(
  database: PostgresDatabase.new,
  mailer: EmailMailer.new
)
```

### 3. Use Modules for Shared Behavior

```ruby
# ✅ GOOD: Module for shared behavior
module Timestampable
  def created_at
    @created_at ||= Time.current
  end
  
  def updated_at=(time)
    @updated_at = time
  end
end

class User
  include Timestampable
end

class Post
  include Timestampable
end
```

### 4. Prefer Composition Over Inheritance

```ruby
# ❌ BAD: Deep inheritance
class Animal; end
class Mammal < Animal; end
class Dog < Mammal; end
class GoldenRetriever < Dog; end  # Too deep!

# ✅ GOOD: Composition
class Dog
  def initialize(breed:, temperament:)
    @breed = breed
    @temperament = temperament
  end
end

golden = Dog.new(
  breed: GoldenRetriever.new,
  temperament: Friendly.new
)
```

### 5. Use Duck Typing

```ruby
# ❌ BAD: Type checking
def process(object)
  if object.is_a?(User)
    object.name
  elsif object.is_a?(Company)
    object.company_name
  end
end

# ✅ GOOD: Duck typing
def process(object)
  object.name  # Any object with #name works
end

# ✅ GOOD: Explicit interface
class User
  def display_name
    name
  end
end

class Company
  def display_name
    company_name
  end
end

def process(object)
  object.display_name
end
```

---

## Functional Programming

### 1. Use map, select, reduce

```ruby
# ❌ BAD: Imperative style
result = []
users.each do |user|
  if user.active?
    result << user.name.upcase
  end
end

# ✅ GOOD: Functional style
result = users
  .select(&:active?)
  .map { |user| user.name.upcase }

# ✅ GOOD: reduce for aggregation
total = orders.reduce(0) { |sum, order| sum + order.amount }

# ✅ BETTER: sum
total = orders.sum(&:amount)
```

### 2. Use Immutable Operations

```ruby
# ❌ BAD: Mutation
def add_prefix(items)
  items.each { |item| item.name = "PREFIX_#{item.name}" }
  items
end

# ✅ GOOD: Return new objects
def add_prefix(items)
  items.map { |item| item.dup.tap { |i| i.name = "PREFIX_#{i.name}" } }
end

# ✅ BETTER: Value objects
User = Struct.new(:name, :email, keyword_init: true) do
  def with_prefix
    self.class.new(name: "PREFIX_#{name}", email: email)
  end
end
```

### 3. Use Blocks Effectively

```ruby
# ✅ GOOD: Custom control structure
def with_timing
  start = Time.current
  result = yield
  duration = Time.current - start
  puts "Took #{duration} seconds"
  result
end

with_timing do
  expensive_operation
end

# ✅ GOOD: Configuration DSL
class Server
  def initialize(&block)
    instance_eval(&block) if block_given?
  end
  
  def port(value)
    @port = value
  end
  
  def host(value)
    @host = value
  end
end

server = Server.new do
  port 3000
  host 'localhost'
end
```

---

## Code Organization

### 1. Use Modules for Namespacing

```ruby
# ✅ GOOD: Hierarchical namespacing
module MyApp
  module Services
    class UserCreator
      # ...
    end
  end
  
  module Models
    class User
      # ...
    end
  end
end

# Usage
user_creator = MyApp::Services::UserCreator.new
user = MyApp::Models::User.new
```

### 2. One Class Per File

```ruby
# ❌ BAD: Multiple classes in one file
# user.rb
class User
  # ...
end

class UserValidator
  # ...
end

# ✅ GOOD: Separate files
# user.rb
class User
  # ...
end

# user_validator.rb
class UserValidator
  # ...
end
```

### 3. Use Concerns (Rails)

```ruby
# app/models/concerns/taggable.rb
module Taggable
  extend ActiveSupport::Concern
  
  included do
    has_many :tags, as: :taggable, dependent: :destroy
    scope :tagged_with, ->(tag_name) { joins(:tags).where(tags: { name: tag_name }) }
  end
  
  def tag_list
    tags.pluck(:name).join(', ')
  end
  
  class_methods do
    def most_tagged
      # Class method available to all including classes
    end
  end
end

# app/models/post.rb
class Post < ApplicationRecord
  include Taggable
end
```

---

## Testing Best Practices

### 1. Follow Arrange-Act-Assert

```ruby
# ✅ GOOD: Clear test structure
RSpec.describe UserCreator do
  describe '#call' do
    it 'creates a user with valid attributes' do
      # Arrange
      attributes = { name: 'John', email: 'john@example.com' }
      creator = UserCreator.new(attributes)
      
      # Act
      result = creator.call
      
      # Assert
      expect(result).to be_a(User)
      expect(result.name).to eq('John')
      expect(result).to be_persisted
    end
  end
end
```

### 2. Use Factories, Not Fixtures

```ruby
# ❌ BAD: Fixtures (brittle)
# test/fixtures/users.yml
john:
  name: John
  email: john@example.com

# ✅ GOOD: Factory (flexible)
# spec/factories/users.rb
FactoryBot.define do
  factory :user do
    name { Faker::Name.name }
    email { Faker::Internet.email }
    
    trait :admin do
      role { :admin }
    end
    
    trait :with_posts do
      after(:create) do |user|
        create_list(:post, 3, user: user)
      end
    end
  end
end

# Usage
user = create(:user)
admin = create(:user, :admin)
user_with_posts = create(:user, :with_posts)
```

### 3. Use let and let! Wisely

```ruby
# ✅ GOOD: Lazy evaluation with let
RSpec.describe User do
  let(:user) { create(:user) }  # Only created when first used
  
  it 'has a name' do
    expect(user.name).to be_present
  end
end

# ✅ GOOD: Eager evaluation with let!
RSpec.describe 'GET /users' do
  let!(:users) { create_list(:user, 3) }  # Created before each test
  
  it 'returns all users' do
    get '/users'
    expect(response.body).to include_json(users.to_json)
  end
end

# ❌ BAD: Complex logic in let
let(:user) do
  user = create(:user)
  user.posts << create(:post)
  user.save!
  user.reload
end  # Too complex!

# ✅ GOOD: Use before block for complex setup
before do
  @user = create(:user)
  @user.posts << create(:post)
  @user.save!
end
```

### 4. Test Behavior, Not Implementation

```ruby
# ❌ BAD: Testing implementation
it 'calls the validate method' do
  expect(user).to receive(:validate)
  user.save
end

# ✅ GOOD: Testing behavior
it 'saves valid users' do
  user = User.new(valid_attributes)
  expect { user.save }.to change { User.count }.by(1)
end

it 'does not save invalid users' do
  user = User.new(invalid_attributes)
  expect { user.save }.not_to change { User.count }
  expect(user.errors).to be_present
end
```

### 5. Use Shared Examples

```ruby
# ✅ GOOD: Shared examples for common behavior
RSpec.shared_examples 'a timestamped model' do
  it 'sets created_at on create' do
    expect(subject.created_at).to be_present
  end
  
  it 'updates updated_at on save' do
    original_time = subject.updated_at
    sleep 0.1
    subject.touch
    expect(subject.updated_at).to be > original_time
  end
end

RSpec.describe User do
  it_behaves_like 'a timestamped model'
end

RSpec.describe Post do
  it_behaves_like 'a timestamped model'
end
```

---

## Common Gotchas

### 1. Frozen String Literals

```ruby
# Ruby 3+: Strings are frozen by default with magic comment
# frozen_string_literal: true

name = 'John'
name << ' Doe'  # FrozenError!

# Solution: Use + or dup
name = 'John'
full_name = name + ' Doe'  # OK

name = 'John'
full_name = name.dup << ' Doe'  # OK
```

### 2. Parallel Assignment Gotcha

```ruby
# ⚠️ GOTCHA: Order matters!
a = 1
b = 2
a, b = b, a
# a => 2, b => 1 (swapped!)

# But:
a = b = 1
# Both are 1, not a swap!
```

### 3. Blocks and Returns

```ruby
# ⚠️ GOTCHA: return in block returns from method!
def find_even_numbers
  [1, 2, 3, 4, 5].select do |n|
    return n if n.even?  # Returns from method, not block!
  end
end

find_even_numbers  # => 2 (method returns, doesn't iterate)

# ✅ CORRECT: Use next or implicit return
def find_even_numbers
  [1, 2, 3, 4, 5].select do |n|
    n.even?  # Implicit return from block
  end
end

find_even_numbers  # => [2, 4]
```

### 4. Hash Default Values

```ruby
# ⚠️ GOTCHA: Default value is shared!
hash = Hash.new([])
hash[:a] << 1
hash[:b] << 2
hash[:a]  # => [1, 2] (same array!)

# ✅ CORRECT: Use block form
hash = Hash.new { |h, k| h[k] = [] }
hash[:a] << 1
hash[:b] << 2
hash[:a]  # => [1]
hash[:b]  # => [2]
```

### 5. String vs Symbol Keys

```ruby
hash = { 'name' => 'John', age: 30 }

hash[:name]   # => nil
hash['name']  # => 'John'
hash[:age]    # => 30
hash['age']   # => nil

# ✅ SOLUTION: Use HashWithIndifferentAccess (Rails)
hash = HashWithIndifferentAccess.new({ 'name' => 'John', age: 30 })
hash[:name]   # => 'John'
hash['name']  # => 'John'
```

### 6. Truthiness

```ruby
# ⚠️ Remember: Only nil and false are falsy!

if 0
  puts 'Zero is truthy!'  # This executes!
end

if ''
  puts 'Empty string is truthy!'  # This executes!
end

if []
  puts 'Empty array is truthy!'  # This executes!
end

# ✅ Explicit checks
if number != 0
  # ...
end

if string.present?  # Rails
  # ...
end

if array.any?
  # ...
end
```

### 7. Integer Division

```ruby
# ⚠️ GOTCHA: Integer division truncates
result = 5 / 2  # => 2, not 2.5

# ✅ SOLUTION: Use float
result = 5.0 / 2  # => 2.5
result = 5 / 2.0  # => 2.5
result = 5.fdiv(2)  # => 2.5
```

### 8. Proc vs Lambda

```ruby
# Different return behavior
def test_proc
  proc { return 'from proc' }.call
  'from method'
end

def test_lambda
  lambda { return 'from lambda' }.call
  'from method'
end

test_proc    # => 'from proc' (returns from method)
test_lambda  # => 'from method' (returns from lambda only)

# Different argument handling
proc = proc { |a, b| [a, b] }
proc.call(1)  # => [1, nil] (lenient)

lam = lambda { |a, b| [a, b] }
lam.call(1)  # ArgumentError (strict)

# ✅ Prefer lambda for predictable behavior
# ✅ Use proc for flexible argument handling
```

---

## Quick Reference

### Enumerable Methods Cheat Sheet

```ruby
# Transformation
array.map { |x| x * 2 }          # [1,2,3] => [2,4,6]
array.flat_map { |x| [x, x*2] }  # [1,2] => [1,2,2,4]

# Filtering
array.select { |x| x.even? }     # [1,2,3,4] => [2,4]
array.reject { |x| x.even? }     # [1,2,3,4] => [1,3]
array.filter { |x| x > 2 }       # [1,2,3,4] => [3,4]

# Aggregation
array.reduce(:+)                 # [1,2,3] => 6
array.sum                        # [1,2,3] => 6
array.min / array.max            # [1,2,3] => 1 / 3

# Searching
array.find { |x| x > 2 }         # [1,2,3,4] => 3
array.any? { |x| x > 2 }         # [1,2,3] => true
array.all? { |x| x > 0 }         # [1,2,3] => true
array.none? { |x| x < 0 }        # [1,2,3] => true

# Grouping
array.group_by { |x| x % 2 }     # {0=>[2,4], 1=>[1,3]}
array.partition { |x| x.even? }  # [[2,4], [1,3]]

# Sorting
array.sort                       # [3,1,2] => [1,2,3]
array.sort_by { |x| -x }         # [1,2,3] => [3,2,1]
```

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial Ruby best practices guide |

---

## References

- [Ruby Style Guide](https://rubystyle.guide/)
- [The Ruby Programming Language](https://www.ruby-lang.org/en/documentation/)
- [Effective Ruby](https://www.effectiveruby.com/)
- [RuboCop Documentation](https://docs.rubocop.org/)
- [RSpec Best Practices](https://www.betterspecs.org/)

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

