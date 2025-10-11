# AI Integration Manual - Ruby

**Version:** 1.0.0  
**Last Updated:** 2025-10-11  
**Target Audience:** AI Agents (LLM-based development assistants)  
**Language:** Ruby (3.0+)

---

## Table of Contents

1. [Introduction](#introduction)
2. [Ruby Environment Setup](#ruby-environment-setup)
3. [Project Structure](#project-structure)
4. [Dependency Management (Bundler)](#dependency-management-bundler)
5. [Testing Framework](#testing-framework)
6. [Code Style & Linting](#code-style--linting)
7. [Documentation (YARD)](#documentation-yard)
8. [Rails Integration](#rails-integration)
9. [Implementation Guidelines](#implementation-guidelines)
10. [Error Handling](#error-handling)
11. [Performance Optimization](#performance-optimization)
12. [Security Best Practices](#security-best-practices)
13. [Continuous Integration](#continuous-integration)
14. [Gem Publishing](#gem-publishing)

---

## Introduction

This manual extends the base AI Integration Manual Template with Ruby-specific practices, tools, and workflows. It focuses on modern Ruby (3.0+) development with emphasis on clarity, expressiveness, and developer happiness.

### Core Principles for Ruby

1. **Convention over Configuration**: Follow Ruby and Rails conventions
2. **DRY (Don't Repeat Yourself)**: Eliminate duplication through abstraction
3. **Explicit is Better**: Clear code over clever code
4. **Test-Driven Development**: Write tests first when possible
5. **Duck Typing**: Focus on object behavior, not class hierarchy
6. **Blocks and Iterators**: Leverage Ruby's functional programming features
7. **Metaprogramming Wisely**: Use when appropriate, avoid over-engineering

---

## Ruby Environment Setup

### Required Tools

```bash
# Install Ruby Version Manager (rbenv - recommended)
git clone https://github.com/rbenv/rbenv.git ~/.rbenv
echo 'export PATH="$HOME/.rbenv/bin:$PATH"' >> ~/.bashrc
echo 'eval "$(rbenv init -)"' >> ~/.bashrc
source ~/.bashrc

# Install ruby-build plugin
git clone https://github.com/rbenv/ruby-build.git ~/.rbenv/plugins/ruby-build

# Alternative: RVM
curl -sSL https://get.rvm.io | bash -s stable
source ~/.rvm/scripts/rvm

# Install Ruby 3.3 (latest stable)
rbenv install 3.3.0
rbenv global 3.3.0

# Or with RVM
rvm install 3.3.0
rvm use 3.3.0 --default

# Verify installation
ruby --version  # Should show Ruby 3.3.0

# Install Bundler
gem install bundler

# Install essential gems
gem install rubocop rubocop-rspec rubocop-performance
gem install rspec
gem install yard
gem install pry pry-byebug  # Debugging
gem install rake
```

### Version Requirements

- **Minimum Ruby Version**: 3.0.0
- **Recommended**: 3.3.x (latest stable)
- **Bundler**: 2.4+
- **RubyGems**: Latest stable

### Environment Configuration

```bash
# .ruby-version file (for rbenv/rvm)
echo "3.3.0" > .ruby-version

# Set default Bundler options
bundle config set --local path 'vendor/bundle'
bundle config set --local jobs 4

# Disable documentation installation for gems (optional)
echo "gem: --no-document" >> ~/.gemrc
```

---

## Project Structure

### Standard Ruby Project Layout

```
project-root/
├── .github/
│   └── workflows/
│       ├── ci.yml
│       └── release.yml
├── .gitignore
├── .rubocop.yml
├── .ruby-version
├── .yardopts
├── Gemfile
├── Gemfile.lock
├── Rakefile
├── README.md
├── CHANGELOG.md
├── LICENSE
├── docs/
│   ├── ROADMAP.md
│   ├── SPECS.md
│   └── specs/
├── lib/
│   ├── project_name.rb           # Main entry point
│   └── project_name/
│       ├── version.rb
│       ├── configuration.rb
│       ├── core.rb
│       └── utils.rb
├── spec/                          # RSpec tests
│   ├── spec_helper.rb
│   ├── project_name_spec.rb
│   └── project_name/
│       ├── core_spec.rb
│       └── utils_spec.rb
├── test/                          # Minitest (alternative)
│   ├── test_helper.rb
│   └── project_name_test.rb
├── bin/
│   ├── console                    # IRB console with project loaded
│   └── setup                      # Setup script
├── exe/                           # Executables
│   └── project_name
└── examples/                      # Usage examples
    └── basic_usage.rb
```

### Rails Project Layout

```
rails-app/
├── app/
│   ├── controllers/
│   ├── models/
│   ├── views/
│   ├── jobs/
│   ├── mailers/
│   └── services/                  # Business logic
├── config/
│   ├── application.rb
│   ├── database.yml
│   ├── routes.rb
│   └── environments/
├── db/
│   ├── migrate/
│   ├── schema.rb
│   └── seeds.rb
├── lib/
│   └── tasks/                     # Rake tasks
├── spec/
│   ├── models/
│   ├── controllers/
│   ├── requests/
│   ├── factories/                 # FactoryBot
│   └── support/
├── public/
├── Gemfile
├── Gemfile.lock
└── config.ru
```

---

## Dependency Management (Bundler)

### Gemfile

```ruby
# frozen_string_literal: true

source 'https://rubygems.org'

# Specify Ruby version
ruby '3.3.0'

# Gem specification file
gemspec

# Development dependencies
group :development do
  gem 'bundler', '~> 2.4'
  gem 'rake', '~> 13.0'
  gem 'rubocop', '~> 1.60'
  gem 'rubocop-performance', '~> 1.20'
  gem 'rubocop-rspec', '~> 2.26'
  gem 'yard', '~> 0.9'
  gem 'pry', '~> 0.14'
  gem 'pry-byebug', '~> 3.10'
end

group :test do
  gem 'rspec', '~> 3.12'
  gem 'simplecov', '~> 0.22', require: false
  gem 'webmock', '~> 3.19'
  gem 'vcr', '~> 6.2'
  gem 'factory_bot', '~> 6.4'
  gem 'faker', '~> 3.2'
end

# Runtime dependencies
gem 'zeitwerk', '~> 2.6'  # Autoloading
gem 'dry-rb', '~> 3.0'    # Functional utilities (optional)
```

### Gemspec (for libraries)

**project_name.gemspec**:
```ruby
# frozen_string_literal: true

require_relative 'lib/project_name/version'

Gem::Specification.new do |spec|
  spec.name          = 'project_name'
  spec.version       = ProjectName::VERSION
  spec.authors       = ['Your Name']
  spec.email         = ['your.email@example.com']

  spec.summary       = 'Brief description of your gem'
  spec.description   = 'Longer description of your gem'
  spec.homepage      = 'https://github.com/username/project_name'
  spec.license       = 'MIT'
  spec.required_ruby_version = '>= 3.0.0'

  spec.metadata['homepage_uri'] = spec.homepage
  spec.metadata['source_code_uri'] = spec.homepage
  spec.metadata['changelog_uri'] = "#{spec.homepage}/blob/main/CHANGELOG.md"
  spec.metadata['documentation_uri'] = "https://rubydoc.info/gems/#{spec.name}"
  spec.metadata['bug_tracker_uri'] = "#{spec.homepage}/issues"

  # Specify which files should be added to the gem
  spec.files = Dir.glob(%w[
    lib/**/*.rb
    exe/*
    README.md
    CHANGELOG.md
    LICENSE
  ]).reject { |f| File.directory?(f) }

  spec.bindir        = 'exe'
  spec.executables   = spec.files.grep(%r{^exe/}) { |f| File.basename(f) }
  spec.require_paths = ['lib']

  # Runtime dependencies
  spec.add_dependency 'zeitwerk', '~> 2.6'

  # Development dependencies
  spec.add_development_dependency 'bundler', '~> 2.4'
  spec.add_development_dependency 'rake', '~> 13.0'
  spec.add_development_dependency 'rspec', '~> 3.12'
  spec.add_development_dependency 'rubocop', '~> 1.60'
end
```

### Bundler Commands

```bash
# Install dependencies
bundle install

# Update dependencies
bundle update

# Check for outdated gems
bundle outdated

# Execute command in bundle context
bundle exec rspec

# Show gem location
bundle show <gem_name>

# Audit for security vulnerabilities
bundle audit check --update

# Clean up unused gems
bundle clean

# List all gems
bundle list
```

---

## Testing Framework

### RSpec Setup

**spec/spec_helper.rb**:
```ruby
# frozen_string_literal: true

require 'simplecov'
SimpleCov.start do
  add_filter '/spec/'
  add_filter '/vendor/'
  minimum_coverage 90
end

require 'project_name'

RSpec.configure do |config|
  # Enable flags like --only-failures and --next-failure
  config.example_status_persistence_file_path = '.rspec_status'

  # Disable RSpec exposing methods globally on `Module` and `main`
  config.disable_monkey_patching!

  # Use expect syntax
  config.expect_with :rspec do |c|
    c.syntax = :expect
  end

  # Randomize test order
  config.order = :random
  Kernel.srand config.seed

  # Run specs in random order to surface order dependencies
  config.profile_examples = 10
end
```

**.rspec**:
```
--require spec_helper
--color
--format documentation
--order random
```

### RSpec Test Example

**spec/project_name/core_spec.rb**:
```ruby
# frozen_string_literal: true

RSpec.describe ProjectName::Core do
  describe '#initialize' do
    it 'creates a new instance with default values' do
      core = described_class.new

      expect(core).to be_a(described_class)
      expect(core.value).to eq(0)
    end

    it 'accepts initialization parameters' do
      core = described_class.new(value: 42)

      expect(core.value).to eq(42)
    end
  end

  describe '#process' do
    subject(:core) { described_class.new(value: 10) }

    context 'with valid input' do
      it 'processes the input correctly' do
        result = core.process(5)

        expect(result).to eq(15)
      end
    end

    context 'with invalid input' do
      it 'raises ArgumentError for negative numbers' do
        expect { core.process(-1) }.to raise_error(ArgumentError, /negative/)
      end

      it 'raises ArgumentError for nil' do
        expect { core.process(nil) }.to raise_error(ArgumentError)
      end
    end
  end

  describe '#reset' do
    it 'resets the value to zero' do
      core = described_class.new(value: 42)
      core.reset

      expect(core.value).to eq(0)
    end
  end
end
```

### RSpec with Rails

**spec/rails_helper.rb**:
```ruby
# frozen_string_literal: true

require 'spec_helper'
ENV['RAILS_ENV'] ||= 'test'
require_relative '../config/environment'
abort('Rails is running in production mode!') if Rails.env.production?
require 'rspec/rails'

# Add additional requires below this line. Rails is not loaded until this point!
require 'factory_bot_rails'
require 'shoulda/matchers'

# Load support files
Dir[Rails.root.join('spec/support/**/*.rb')].sort.each { |f| require f }

# Checks for pending migrations and applies them before tests are run
begin
  ActiveRecord::Migration.maintain_test_schema!
rescue ActiveRecord::PendingMigrationError => e
  abort e.to_s.strip
end

RSpec.configure do |config|
  # Use transactional fixtures
  config.use_transactional_fixtures = true

  # Infer spec type from file location
  config.infer_spec_type_from_file_location!

  # Filter lines from Rails gems in backtraces
  config.filter_rails_from_backtrace!

  # Include FactoryBot methods
  config.include FactoryBot::Syntax::Methods

  # Database Cleaner
  config.before(:suite) do
    DatabaseCleaner.strategy = :transaction
    DatabaseCleaner.clean_with(:truncation)
  end

  config.around(:each) do |example|
    DatabaseCleaner.cleaning do
      example.run
    end
  end
end

# Shoulda Matchers configuration
Shoulda::Matchers.configure do |config|
  config.integrate do |with|
    with.test_framework :rspec
    with.library :rails
  end
end
```

### Minitest (Alternative)

**test/test_helper.rb**:
```ruby
# frozen_string_literal: true

$LOAD_PATH.unshift File.expand_path('../lib', __dir__)
require 'project_name'

require 'minitest/autorun'
require 'minitest/pride'
require 'simplecov'

SimpleCov.start do
  add_filter '/test/'
end
```

**test/project_name_test.rb**:
```ruby
# frozen_string_literal: true

require 'test_helper'

class ProjectNameTest < Minitest::Test
  def test_that_it_has_a_version_number
    refute_nil ::ProjectName::VERSION
  end

  def test_it_does_something_useful
    assert true
  end
end
```

### Running Tests

```bash
# RSpec
bundle exec rspec

# Run specific file
bundle exec rspec spec/project_name/core_spec.rb

# Run specific line
bundle exec rspec spec/project_name/core_spec.rb:10

# Run with coverage
COVERAGE=true bundle exec rspec

# Run with profiling
bundle exec rspec --profile 10

# Minitest
bundle exec rake test

# Run specific test
bundle exec ruby test/project_name_test.rb
```

---

## Code Style & Linting

### RuboCop Configuration

**.rubocop.yml**:
```yaml
require:
  - rubocop-performance
  - rubocop-rspec

AllCops:
  TargetRubyVersion: 3.3
  NewCops: enable
  Exclude:
    - 'vendor/**/*'
    - 'db/schema.rb'
    - 'db/migrate/*.rb'
    - 'bin/*'
    - 'node_modules/**/*'

# Layout
Layout/LineLength:
  Max: 120
  Exclude:
    - 'spec/**/*'

Layout/MultilineMethodCallIndentation:
  EnforcedStyle: indented

# Style
Style/Documentation:
  Enabled: true
  Exclude:
    - 'spec/**/*'
    - 'test/**/*'

Style/StringLiterals:
  EnforcedStyle: single_quotes

Style/StringLiteralsInInterpolation:
  EnforcedStyle: single_quotes

Style/FrozenStringLiteralComment:
  Enabled: true
  EnforcedStyle: always

Style/HashSyntax:
  EnforcedStyle: ruby19
  EnforcedShorthandSyntax: either

Style/SymbolArray:
  EnforcedStyle: brackets

Style/WordArray:
  EnforcedStyle: brackets

# Metrics
Metrics/BlockLength:
  Exclude:
    - 'spec/**/*'
    - 'test/**/*'
    - 'config/**/*'
    - 'Rakefile'

Metrics/MethodLength:
  Max: 25
  Exclude:
    - 'spec/**/*'

Metrics/ClassLength:
  Max: 150

Metrics/ModuleLength:
  Max: 150

# Naming
Naming/FileName:
  Exclude:
    - 'Gemfile'
    - 'Rakefile'

# RSpec
RSpec/ExampleLength:
  Max: 15

RSpec/MultipleExpectations:
  Max: 5

RSpec/NestedGroups:
  Max: 5

# Performance
Performance/TimesMap:
  Enabled: true
```

### Running RuboCop

```bash
# Check all files
bundle exec rubocop

# Auto-correct safe offenses
bundle exec rubocop -a

# Auto-correct all offenses (including unsafe)
bundle exec rubocop -A

# Check specific file
bundle exec rubocop lib/project_name/core.rb

# Generate TODO file for existing offenses
bundle exec rubocop --auto-gen-config

# Run with specific cops
bundle exec rubocop --only Layout/LineLength
```

### Code Formatting Script

**bin/format**:
```bash
#!/usr/bin/env bash
set -e

echo "Running RuboCop auto-correct..."
bundle exec rubocop -a

echo "Code formatting complete!"
```

```bash
chmod +x bin/format
./bin/format
```

---

## Documentation (YARD)

### YARD Configuration

**.yardopts**:
```
--markup markdown
--no-private
--protected
--output-dir docs/api
lib/**/*.rb
-
README.md
CHANGELOG.md
LICENSE
```

### Documentation Standards

**lib/project_name/core.rb**:
```ruby
# frozen_string_literal: true

module ProjectName
  # Core functionality for processing data
  #
  # This class provides the main business logic for the project.
  # It handles data processing, validation, and transformation.
  #
  # @example Basic usage
  #   core = Core.new(value: 42)
  #   result = core.process(10)
  #   puts result #=> 52
  #
  # @example With error handling
  #   core = Core.new
  #   begin
  #     core.process(-1)
  #   rescue ArgumentError => e
  #     puts "Error: #{e.message}"
  #   end
  #
  # @author Your Name
  # @since 1.0.0
  class Core
    # @return [Integer] the current value
    attr_reader :value

    # Initialize a new Core instance
    #
    # @param value [Integer] the initial value (default: 0)
    # @raise [ArgumentError] if value is not an Integer
    #
    # @example
    #   core = Core.new(value: 42)
    def initialize(value: 0)
      raise ArgumentError, 'value must be an Integer' unless value.is_a?(Integer)

      @value = value
    end

    # Process an input value
    #
    # This method adds the input to the current value and returns the result.
    #
    # @param input [Integer] the value to add
    # @return [Integer] the sum of value and input
    # @raise [ArgumentError] if input is negative or not an Integer
    #
    # @example
    #   core = Core.new(value: 10)
    #   core.process(5) #=> 15
    #
    # @see #reset
    def process(input)
      raise ArgumentError, 'input must be an Integer' unless input.is_a?(Integer)
      raise ArgumentError, 'input cannot be negative' if input.negative?

      @value + input
    end

    # Reset the value to zero
    #
    # @return [Integer] the new value (0)
    #
    # @example
    #   core = Core.new(value: 42)
    #   core.reset #=> 0
    def reset
      @value = 0
    end
  end
end
```

### Generating Documentation

```bash
# Generate YARD documentation
bundle exec yard doc

# Generate and open in browser
bundle exec yard server

# Generate with statistics
bundle exec yard stats --list-undoc

# Generate with coverage
bundle exec yard stats --list-undoc --compact
```

---

## Rails Integration

### Rails New Project

```bash
# Create new Rails app
rails new myapp --database=postgresql --css=tailwind --javascript=esbuild

# Or API-only
rails new myapp --api --database=postgresql

# With RSpec instead of Minitest
rails new myapp --skip-test --database=postgresql

# Install RSpec
bundle add rspec-rails --group development,test
rails generate rspec:install
```

### Rails Best Practices

**app/services/user_creator.rb**:
```ruby
# frozen_string_literal: true

module Users
  # Service object for creating users
  #
  # This service handles user creation with validation,
  # email sending, and logging.
  #
  # @example
  #   result = Users::Creator.call(email: 'user@example.com', name: 'John')
  #   if result.success?
  #     user = result.user
  #   else
  #     errors = result.errors
  #   end
  class Creator
    # @param attributes [Hash] user attributes
    # @option attributes [String] :email user's email
    # @option attributes [String] :name user's name
    def initialize(attributes)
      @attributes = attributes
    end

    # Create a user
    #
    # @return [Result] result object with user or errors
    def call
      user = User.new(@attributes)

      if user.save
        send_welcome_email(user)
        log_creation(user)
        Result.success(user: user)
      else
        Result.failure(errors: user.errors)
      end
    rescue StandardError => e
      Rails.logger.error("User creation failed: #{e.message}")
      Result.failure(errors: [e.message])
    end

    # Convenience class method
    def self.call(attributes)
      new(attributes).call
    end

    private

    def send_welcome_email(user)
      UserMailer.welcome(user).deliver_later
    end

    def log_creation(user)
      Rails.logger.info("User created: #{user.id}")
    end
  end

  # Result object for service calls
  class Result
    attr_reader :user, :errors

    def initialize(success:, user: nil, errors: [])
      @success = success
      @user = user
      @errors = errors
    end

    def success?
      @success
    end

    def failure?
      !@success
    end

    def self.success(user:)
      new(success: true, user: user)
    end

    def self.failure(errors:)
      new(success: false, errors: errors)
    end
  end
end
```

### Rails Controller Example

**app/controllers/api/v1/users_controller.rb**:
```ruby
# frozen_string_literal: true

module Api
  module V1
    # Users API controller
    class UsersController < ApplicationController
      before_action :set_user, only: [:show, :update, :destroy]

      # GET /api/v1/users
      def index
        users = User.page(params[:page]).per(params[:per_page] || 25)

        render json: users, status: :ok
      end

      # GET /api/v1/users/:id
      def show
        render json: @user, status: :ok
      end

      # POST /api/v1/users
      def create
        result = Users::Creator.call(user_params)

        if result.success?
          render json: result.user, status: :created
        else
          render json: { errors: result.errors }, status: :unprocessable_entity
        end
      end

      # PATCH/PUT /api/v1/users/:id
      def update
        if @user.update(user_params)
          render json: @user, status: :ok
        else
          render json: { errors: @user.errors }, status: :unprocessable_entity
        end
      end

      # DELETE /api/v1/users/:id
      def destroy
        @user.destroy
        head :no_content
      end

      private

      def set_user
        @user = User.find(params[:id])
      rescue ActiveRecord::RecordNotFound
        render json: { error: 'User not found' }, status: :not_found
      end

      def user_params
        params.require(:user).permit(:email, :name, :password, :password_confirmation)
      end
    end
  end
end
```

---

## Implementation Guidelines

### Ruby Idioms

```ruby
# Use symbols for hash keys
user = { name: 'John', age: 30 }

# Use safe navigation operator
user&.profile&.address

# Use double-splat for keyword arguments
def create_user(**attributes)
  User.create(**attributes)
end

# Use trailing conditionals for guards
return unless user.valid?
raise ArgumentError, 'Invalid user' if user.nil?

# Use ||= for memoization
def expensive_calculation
  @result ||= complex_operation
end

# Use tap for object configuration
user = User.new.tap do |u|
  u.name = 'John'
  u.email = 'john@example.com'
end

# Use Array#each instead of for loops
names.each do |name|
  puts name
end

# Use Array#map for transformations
upper_names = names.map(&:upcase)

# Use Array#select for filtering
adults = users.select { |u| u.age >= 18 }

# Use blocks for resource management
File.open('data.txt', 'r') do |file|
  file.read
end  # File automatically closed
```

### Object-Oriented Design

```ruby
# frozen_string_literal: true

# Use modules for namespacing
module MyApp
  module Services
    # Service class
    class UserService
      def initialize(user_repository)
        @user_repository = user_repository
      end

      def find_active_users
        @user_repository.where(active: true)
      end
    end
  end
end

# Use concerns for shared behavior
module Timestampable
  extend ActiveSupport::Concern

  included do
    before_save :set_timestamps
  end

  private

  def set_timestamps
    now = Time.current
    self.updated_at = now
    self.created_at ||= now
  end
end

# Use inheritance sparingly
class Animal
  def speak
    raise NotImplementedError, 'Subclass must implement speak'
  end
end

class Dog < Animal
  def speak
    'Woof!'
  end
end

# Prefer composition over inheritance
class User
  def initialize(notifier:, logger:)
    @notifier = notifier
    @logger = logger
  end

  def create
    # Use injected dependencies
    @notifier.send_welcome
    @logger.info('User created')
  end
end
```

---

## Error Handling

### Custom Exceptions

```ruby
# frozen_string_literal: true

module ProjectName
  # Base error class
  class Error < StandardError; end

  # Specific error classes
  class ValidationError < Error
    attr_reader :field

    def initialize(field, message)
      @field = field
      super("Validation failed for #{field}: #{message}")
    end
  end

  class NotFoundError < Error; end
  class UnauthorizedError < Error; end
  class ConfigurationError < Error; end
end
```

### Error Handling Patterns

```ruby
# Use rescue with specific exceptions
def process_data(data)
  validate!(data)
  transform(data)
rescue ValidationError => e
  logger.error("Validation failed: #{e.message}")
  nil
rescue StandardError => e
  logger.error("Unexpected error: #{e.message}")
  raise
end

# Use rescue modifier for simple cases
result = risky_operation rescue default_value

# Use ensure for cleanup
def process_file(filename)
  file = File.open(filename)
  process(file)
ensure
  file&.close
end

# Use throw/catch for control flow (not exceptions)
def find_in_nested_structure(structure, value)
  catch(:found) do
    structure.each do |item|
      throw(:found, item) if item == value
    end
    nil
  end
end
```

---

## Performance Optimization

### Benchmarking

```ruby
require 'benchmark'

# Simple benchmark
time = Benchmark.realtime do
  expensive_operation
end
puts "Took #{time} seconds"

# Compare multiple approaches
Benchmark.bm(20) do |x|
  x.report('Array#map:') { 1000.times { (1..100).map { |n| n * 2 } } }
  x.report('Array#each:') { 1000.times { result = []; (1..100).each { |n| result << n * 2 } } }
end

# Memory profiling
require 'memory_profiler'

report = MemoryProfiler.report do
  expensive_operation
end

report.pretty_print
```

### Optimization Techniques

```ruby
# Use symbols instead of strings for keys
hash = { name: 'John' }  # Good
hash = { 'name' => 'John' }  # Less efficient

# Avoid creating unnecessary objects
# Bad
1000.times do
  user.name + ' ' + user.surname
end

# Good
full_name = "#{user.name} #{user.surname}"
1000.times { full_name }

# Use lazy enumerators for large collections
(1..Float::INFINITY)
  .lazy
  .select(&:even?)
  .take(10)
  .to_a

# Memoize expensive computations
def expensive_result
  @expensive_result ||= complex_calculation
end

# Use bulk operations
# Bad
users.each { |user| user.update(active: true) }

# Good
User.where(id: user_ids).update_all(active: true)
```

---

## Security Best Practices

### Input Validation

```ruby
# Validate with strong parameters (Rails)
def user_params
  params.require(:user).permit(:name, :email, :age)
end

# Custom validation
def validate_email(email)
  raise ValidationError.new(:email, 'invalid format') unless email.match?(/\A[\w+\-.]+@[a-z\d\-]+(\.[a-z\d\-]+)*\.[a-z]+\z/i)
end

# Sanitize HTML
require 'sanitize'

clean_html = Sanitize.fragment(user_input, Sanitize::Config::RELAXED)
```

### SQL Injection Prevention

```ruby
# ❌ Bad: String interpolation
User.where("email = '#{email}'")  # Vulnerable!

# ✅ Good: Parameterized queries
User.where('email = ?', email)
User.where(email: email)

# ✅ Good: Named parameters
User.where('email = :email AND active = :active', email: email, active: true)
```

### Secrets Management

```ruby
# Use Rails credentials
Rails.application.credentials.secret_key_base

# Use environment variables
ENV.fetch('API_KEY') { raise 'API_KEY not set' }

# Use dotenv in development
# Gemfile
gem 'dotenv-rails', groups: [:development, :test]

# .env
API_KEY=your_key_here
DATABASE_URL=postgres://localhost/mydb
```

---

## Continuous Integration

### GitHub Actions Workflow

**.github/workflows/ci.yml**:
```yaml
name: Ruby CI

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
        ruby-version: ['3.0', '3.1', '3.2', '3.3']

    steps:
      - uses: actions/checkout@v3

      - name: Set up Ruby
        uses: ruby/setup-ruby@v1
        with:
          ruby-version: ${{ matrix.ruby-version }}
          bundler-cache: true

      - name: Install dependencies
        run: bundle install

      - name: Run RuboCop
        run: bundle exec rubocop

      - name: Run tests
        run: bundle exec rspec

      - name: Upload coverage
        uses: codecov/codecov-action@v3
        if: matrix.ruby-version == '3.3'
        with:
          files: ./coverage/coverage.xml

  security:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3

      - name: Set up Ruby
        uses: ruby/setup-ruby@v1
        with:
          ruby-version: '3.3'
          bundler-cache: true

      - name: Run Brakeman
        run: |
          gem install brakeman
          brakeman --no-pager

      - name: Run bundle audit
        run: |
          gem install bundler-audit
          bundle audit check --update
```

---

## Gem Publishing

### Preparing for Release

```bash
# Update version
# lib/project_name/version.rb
module ProjectName
  VERSION = '1.0.0'
end

# Update CHANGELOG
# Add release notes

# Build gem
bundle exec rake build

# Check gem contents
gem spec pkg/project_name-1.0.0.gem
```

### Publishing to RubyGems

```bash
# Create RubyGems account (if needed)
# https://rubygems.org/sign_up

# Login
gem push pkg/project_name-1.0.0.gem

# Or use Rake task
bundle exec rake release
```

### Rakefile for Publishing

**Rakefile**:
```ruby
# frozen_string_literal: true

require 'bundler/gem_tasks'
require 'rspec/core/rake_task'
require 'rubocop/rake_task'

RSpec::Core::RakeTask.new(:spec)
RuboCop::RakeTask.new

task default: [:rubocop, :spec]

desc 'Run console with gem loaded'
task :console do
  require 'irb'
  require 'project_name'
  ARGV.clear
  IRB.start
end
```

---

## Quick Reference

### Essential Commands

```bash
# Setup
bundle install

# Run tests
bundle exec rspec

# Lint code
bundle exec rubocop -a

# Generate docs
bundle exec yard doc

# Start console
bin/console

# Run Rails server
rails server

# Run Rails console
rails console

# Database operations
rails db:create db:migrate db:seed

# Generate code
rails generate model User name:string
rails generate controller Users
rails generate migration AddAgeToUsers age:integer
```

### Checklist for Implementation

- [ ] Ruby 3.0+ installed
- [ ] Gemfile and gemspec configured
- [ ] RSpec or Minitest setup
- [ ] RuboCop configured
- [ ] YARD documentation configured
- [ ] Tests written and passing (>90% coverage)
- [ ] Code linted and formatted
- [ ] Documentation generated
- [ ] CHANGELOG updated
- [ ] Version bumped
- [ ] Security audit passed

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial Ruby manual creation |

---

## References

- [Ruby Style Guide](https://rubystyle.guide/)
- [RSpec Best Practices](https://www.betterspecs.org/)
- [RuboCop Documentation](https://docs.rubocop.org/)
- [YARD Documentation](https://yardoc.org/)
- [Rails Guides](https://guides.rubyonrails.org/)

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

