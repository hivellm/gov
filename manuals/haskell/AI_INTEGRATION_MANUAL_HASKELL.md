# AI Integration Manual - Haskell

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Language**: Haskell (GHC 9.0+)  
**Target Audience**: AI Agents (LLM-based development assistants)

---

## Table of Contents

1. [Introduction](#introduction)
2. [Quick Start](#quick-start)
3. [Haskell-Specific Setup](#haskell-specific-setup)
4. [Configuration Standards](#configuration-standards)
5. [Source Code Standards](#source-code-standards)
6. [Testing Standards](#testing-standards)
7. [Build & Deployment](#build--deployment)
8. [Documentation](#documentation)
9. [Haskell Best Practices](#haskell-best-practices)
10. [Common Patterns](#common-patterns)
11. [Troubleshooting](#troubleshooting)
12. [Complete Workflow](#complete-workflow)

---

## Introduction

This manual extends the [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md) with Haskell-specific implementations.

**When to use this manual**:
- Type-safe backend services and APIs
- Compilers and interpreters
- Financial systems and trading platforms
- Data processing pipelines
- Domain-specific languages (DSLs)
- Mathematical and scientific computing
- CLI tools and utilities
- Libraries requiring strong correctness guarantees

**Prerequisites**:
- Read [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md)
- Read [Language Standards](../LANGUAGE_STANDARDS.md)
- Basic Haskell knowledge

**Key Haskell Characteristics**:
- Pure functional programming
- Strong static typing with type inference
- Lazy evaluation by default
- Algebraic data types
- Type classes and polymorphism
- Monad-based effects
- Referential transparency
- Immutability by default

---

## Quick Start

### Minimum Viable Project with Stack

```bash
# 1. Create project
stack new my-project simple

# 2. Navigate to project
cd my-project

# 3. Build
stack build

# 4. Run
stack exec my-project

# 5. Test
stack test
```

### Minimum Viable Project with Cabal

```bash
# 1. Create project
cabal init --simple --package-name=my-project

# 2. Build
cabal build

# 3. Run
cabal run

# 4. Test
cabal test
```

### Web API Quick Start (Servant)

```bash
# Create project
stack new my-api servant

cd my-api

# Install dependencies
stack build

# Run server
stack exec my-api-exe
```

---

## Haskell-Specific Setup

### 1. Install GHC and Build Tools

**Using GHCup (Recommended)**:

```bash
# Install GHCup
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Restart terminal or source
source ~/.ghcup/env

# Install GHC
ghcup install ghc 9.6.3
ghcup set ghc 9.6.3

# Install Cabal
ghcup install cabal latest
ghcup set cabal latest

# Install Stack
ghcup install stack latest
ghcup set stack latest

# Install HLS (Haskell Language Server)
ghcup install hls latest

# Verify
ghc --version
cabal --version
stack --version
haskell-language-server-wrapper --version
```

**Using Stack (Alternative)**:

```bash
# Install Stack
curl -sSL https://get.haskellstack.org/ | sh

# Install GHC via Stack
stack setup

# Verify
stack --version
stack ghc -- --version
```

### 2. Editor Setup

**VS Code** (Recommended):

```bash
# Install Haskell extension
code --install-extension haskell.haskell
```

**Settings** (`.vscode/settings.json`):

```json
{
  "haskell.manageHLS": "GHCup",
  "haskell.serverExecutablePath": "haskell-language-server-wrapper",
  "haskell.formattingProvider": "ormolu",
  "[haskell]": {
    "editor.formatOnSave": true,
    "editor.defaultFormatter": "haskell.haskell"
  }
}
```

### 3. Essential Tools

```bash
# Ormolu - Formatter
cabal install ormolu

# HLint - Linter
cabal install hlint

# Hoogle - Documentation search
cabal install hoogle
hoogle generate

# Stan - Static analyzer
cabal install stan

# ghcid - Fast reloading
cabal install ghcid
```

---

## Configuration Standards

### 1. package.yaml (Stack Projects)

**Complete Configuration**:

```yaml
name: my-project
version: 0.1.0.0
github: "username/my-project"
license: MIT
author: "Your Name"
maintainer: "email@example.com"
copyright: "2025 Your Name"

extra-source-files:
  - README.md
  - CHANGELOG.md

synopsis: Short description
category: Web

description: |
  Longer description of the project.
  Can span multiple lines.

dependencies:
  - base >= 4.14 && < 5
  - aeson >= 2.0
  - text >= 1.2
  - bytestring >= 0.11
  - containers >= 0.6
  - mtl >= 2.2
  - transformers >= 0.5

ghc-options:
  - -Wall
  - -Wcompat
  - -Widentities
  - -Wincomplete-record-updates
  - -Wincomplete-uni-patterns
  - -Wmissing-export-lists
  - -Wmissing-home-modules
  - -Wpartial-fields
  - -Wredundant-constraints
  - -Werror

default-extensions:
  - OverloadedStrings
  - LambdaCase
  - RecordWildCards
  - DeriveGeneric
  - DeriveFunctor
  - GeneralizedNewtypeDeriving
  - StrictData

library:
  source-dirs: src
  exposed-modules:
    - MyProject.Core
    - MyProject.Types
    - MyProject.Utils

executables:
  my-project-exe:
    main: Main.hs
    source-dirs: app
    ghc-options:
      - -threaded
      - -rtsopts
      - -with-rtsopts=-N
    dependencies:
      - my-project

tests:
  my-project-test:
    main: Spec.hs
    source-dirs: test
    ghc-options:
      - -threaded
      - -rtsopts
      - -with-rtsopts=-N
    dependencies:
      - my-project
      - hspec >= 2.9
      - QuickCheck >= 2.14
      - hspec-discover >= 2.9

benchmarks:
  my-project-bench:
    main: Main.hs
    source-dirs: bench
    dependencies:
      - my-project
      - criterion >= 1.5
```

### 2. .cabal File (Cabal Projects)

**Complete Configuration**:

```cabal
cabal-version: 3.0
name: my-project
version: 0.1.0.0
synopsis: Short description
description: Longer description
license: MIT
license-file: LICENSE
author: Your Name
maintainer: email@example.com
copyright: 2025 Your Name
category: Web
build-type: Simple

common warnings
    ghc-options: -Wall
                 -Wcompat
                 -Widentities
                 -Wincomplete-record-updates
                 -Wincomplete-uni-patterns
                 -Wmissing-export-lists
                 -Wmissing-home-modules
                 -Wpartial-fields
                 -Wredundant-constraints

library
    import: warnings
    exposed-modules:
        MyProject.Core
        MyProject.Types
        MyProject.Utils
    build-depends:
        base >=4.14 && <5,
        aeson >=2.0,
        text >=1.2,
        bytestring >=0.11,
        containers >=0.6
    hs-source-dirs: src
    default-language: Haskell2010
    default-extensions:
        OverloadedStrings
        LambdaCase
        RecordWildCards

executable my-project-exe
    import: warnings
    main-is: Main.hs
    build-depends:
        base >=4.14 && <5,
        my-project
    hs-source-dirs: app
    default-language: Haskell2010
    ghc-options: -threaded -rtsopts -with-rtsopts=-N

test-suite my-project-test
    import: warnings
    default-language: Haskell2010
    type: exitcode-stdio-1.0
    hs-source-dirs: test
    main-is: Spec.hs
    build-depends:
        base >=4.14 && <5,
        my-project,
        hspec >=2.9,
        QuickCheck >=2.14
    ghc-options: -threaded -rtsopts -with-rtsopts=-N
```

### 3. stack.yaml

```yaml
resolver: lts-21.17  # GHC 9.4.7

packages:
  - .

extra-deps: []

flags: {}

ghc-options:
  "$everything": -haddock
```

### 4. .hlint.yaml

**HLint Configuration**:

```yaml
- ignore: {name: "Use camelCase"}
- ignore: {name: "Redundant do"}

- warn: {name: Use newtype instead of data}
- warn: {name: Use section}
- warn: {name: Use if}

- error: {name: Avoid lambda}
- error: {name: Eta reduce}

- functions:
  - {name: unsafePerformIO, within: []}
  - {name: unsafeCoerce, within: []}
```

### 5. .gitignore

```gitignore
# Stack
.stack-work/
stack.yaml.lock

# Cabal
dist/
dist-newstyle/
cabal.project.local
cabal.project.local~

# GHC
*.hi
*.o
*.dyn_hi
*.dyn_o
*.prof
*.aux
*.hp
*.eventlog

# IDE
.vscode/
.idea/
*.swp
*.swo
*~

# OS
.DS_Store
Thumbs.db
```

### 6. .editorconfig

```ini
root = true

[*]
charset = utf-8
end_of_line = lf
insert_final_newline = true
trim_trailing_whitespace = true

[*.hs]
indent_style = space
indent_size = 2
max_line_length = 100

[*.cabal]
indent_style = space
indent_size = 2
```

---

## Source Code Standards

### 1. Directory Structure

```
my-project/
├── app/
│   └── Main.hs              # Executable entry point
├── src/
│   ├── MyProject/
│   │   ├── Core.hs          # Core functionality
│   │   ├── Types.hs         # Type definitions
│   │   ├── API.hs           # API definitions
│   │   ├── Server.hs        # Server implementation
│   │   ├── Database/
│   │   │   ├── Query.hs
│   │   │   └── Schema.hs
│   │   └── Utils.hs         # Utility functions
│   └── Lib.hs               # Library entry
├── test/
│   ├── Spec.hs              # Test entry point
│   ├── MyProject/
│   │   ├── CoreSpec.hs
│   │   └── UtilsSpec.hs
│   └── TestUtils.hs
├── bench/
│   └── Main.hs              # Benchmarks
├── docs/
│   ├── ROADMAP.md
│   └── SPECS.md
├── package.yaml
├── stack.yaml
├── .hlint.yaml
├── LICENSE
└── README.md
```

### 2. Module Structure

**MyProject/Types.hs**:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{-|
Module      : MyProject.Types
Description : Core type definitions
Copyright   : (c) 2025 Your Name
License     : MIT
Maintainer  : email@example.com
Stability   : experimental

This module contains the core type definitions for the project.
-}

module MyProject.Types
  ( -- * User Types
    User (..)
  , UserId (..)
  , UserName (..)
    
    -- * API Types
  , ApiRequest (..)
  , ApiResponse (..)
  
    -- * Error Types
  , AppError (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | User identifier newtype wrapper
newtype UserId = UserId
  { unUserId :: Int
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | User name newtype wrapper
newtype UserName = UserName
  { unUserName :: Text
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | User data type
data User = User
  { userId :: UserId
  , userName :: UserName
  , userEmail :: Text
  , userAge :: Maybe Int
  } deriving (Eq, Show, Generic)

instance FromJSON User
instance ToJSON User

-- | API request type
data ApiRequest
  = GetUser UserId
  | CreateUser Text Text (Maybe Int)
  | UpdateUser UserId Text
  | DeleteUser UserId
  deriving (Eq, Show)

-- | API response type
data ApiResponse
  = UserResponse User
  | SuccessResponse Text
  | ErrorResponse AppError
  deriving (Eq, Show, Generic)

instance ToJSON ApiResponse

-- | Application error type
data AppError
  = NotFound Text
  | ValidationError Text
  | DatabaseError Text
  | UnknownError Text
  deriving (Eq, Show, Generic)

instance ToJSON AppError
```

### 3. Core Business Logic

**MyProject/Core.hs**:

```haskell
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : MyProject.Core
Description : Core business logic
Copyright   : (c) 2025 Your Name
License     : MIT
-}

module MyProject.Core
  ( -- * User Operations
    getUser
  , createUser
  , updateUser
  , deleteUser
  
    -- * Validation
  , validateEmail
  , validateAge
  ) where

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T

import MyProject.Types
  ( AppError (..)
  , User (..)
  , UserId (..)
  , UserName (..)
  )

-- | Get user by ID
getUser ::
  (MonadIO m) =>
  UserId ->
  ExceptT AppError m User
getUser userId = do
  -- Database query would go here
  liftIO $ putStrLn $ "Getting user: " ++ show userId
  
  -- For demo, return mock user
  pure $ User
    { userId = userId
    , userName = UserName "Test User"
    , userEmail = "test@example.com"
    , userAge = Just 25
    }

-- | Create new user
createUser ::
  (MonadIO m) =>
  Text ->
  Text ->
  Maybe Int ->
  ExceptT AppError m User
createUser name email age = do
  -- Validate inputs
  validateEmail email
  case age of
    Just a -> validateAge a
    Nothing -> pure ()
  
  -- Create user
  let user = User
        { userId = UserId 1
        , userName = UserName name
        , userEmail = email
        , userAge = age
        }
  
  liftIO $ putStrLn $ "Creating user: " ++ show user
  pure user

-- | Update user
updateUser ::
  (MonadIO m) =>
  UserId ->
  Text ->
  ExceptT AppError m User
updateUser userId newName = do
  -- Get existing user
  user <- getUser userId
  
  -- Update name
  let updatedUser = user { userName = UserName newName }
  
  liftIO $ putStrLn $ "Updating user: " ++ show userId
  pure updatedUser

-- | Delete user
deleteUser ::
  (MonadIO m) =>
  UserId ->
  ExceptT AppError m ()
deleteUser userId = do
  -- Check if user exists
  _ <- getUser userId
  
  -- Delete user
  liftIO $ putStrLn $ "Deleting user: " ++ show userId

-- | Validate email format
validateEmail :: (Monad m) => Text -> ExceptT AppError m ()
validateEmail email =
  if "@" `T.isInfixOf` email
    then pure ()
    else throwError $ ValidationError "Invalid email format"

-- | Validate age range
validateAge :: (Monad m) => Int -> ExceptT AppError m ()
validateAge age =
  if age >= 0 && age <= 150
    then pure ()
    else throwError $ ValidationError "Invalid age range"
```

### 4. Servant API Example

**MyProject/API.hs**:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module MyProject.API (API, api) where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Servant.API
  ( Capture
  , Delete
  , Get
  , JSON
  , Post
  , Put
  , ReqBody
  , type (:>)
  , type (:<|>)
  )

import MyProject.Types (User, UserId)

-- | API type definition
type API =
       "users" :> Get '[JSON] [User]
  :<|> "users" :> Capture "id" UserId :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] User
  :<|> "users" :> Capture "id" UserId :> ReqBody '[JSON] Text :> Put '[JSON] User
  :<|> "users" :> Capture "id" UserId :> Delete '[JSON] ()

-- | API proxy
api :: Proxy API
api = Proxy
```

### 5. Server Implementation

**MyProject/Server.hs**:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module MyProject.Server (server, app) where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Network.Wai (Application)
import Servant
  ( Handler
  , Server
  , err404
  , err500
  , errBody
  , serve
  , throwError
  , type (:<|>) (..)
  )

import MyProject.API (API, api)
import MyProject.Core
  ( createUser
  , deleteUser
  , getUser
  , updateUser
  )
import MyProject.Types (AppError (..), User, UserId)

-- | Server implementation
server :: Server API
server =
       getAllUsers
  :<|> getUserById
  :<|> createUserHandler
  :<|> updateUserHandler
  :<|> deleteUserHandler

-- | Get all users handler
getAllUsers :: Handler [User]
getAllUsers = do
  liftIO $ putStrLn "Getting all users"
  pure []

-- | Get user by ID handler
getUserById :: UserId -> Handler User
getUserById userId = do
  result <- liftIO $ runExceptT $ getUser userId
  case result of
    Left (NotFound msg) ->
      throwError $ err404 { errBody = show msg }
    Left err ->
      throwError $ err500 { errBody = show err }
    Right user ->
      pure user

-- | Create user handler
createUserHandler :: User -> Handler User
createUserHandler user = do
  liftIO $ putStrLn $ "Creating user: " ++ show user
  pure user

-- | Update user handler
updateUserHandler :: UserId -> Text -> Handler User
updateUserHandler userId newName = do
  result <- liftIO $ runExceptT $ updateUser userId newName
  case result of
    Left err ->
      throwError $ err500 { errBody = show err }
    Right user ->
      pure user

-- | Delete user handler
deleteUserHandler :: UserId -> Handler ()
deleteUserHandler userId = do
  result <- liftIO $ runExceptT $ deleteUser userId
  case result of
    Left err ->
      throwError $ err500 { errBody = show err }
    Right () ->
      pure ()

-- | WAI Application
app :: Application
app = serve api server
```

### 6. Entry Point

**app/Main.hs**:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Wai.Handler.Warp (run)
import System.IO (hPutStrLn, stderr)

import MyProject.Server (app)

-- | Application entry point
main :: IO ()
main = do
  let port = 8080
  hPutStrLn stderr $ "Starting server on port " ++ show port
  run port app
```

---

## Testing Standards

### 1. Test Structure

```
test/
├── Spec.hs                  # Test entry point
├── MyProject/
│   ├── CoreSpec.hs          # Core logic tests
│   ├── TypesSpec.hs         # Type tests
│   └── UtilsSpec.hs         # Utility tests
└── TestUtils.hs             # Test helpers
```

### 2. HSpec Tests

**test/Spec.hs**:

```haskell
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

**test/MyProject/CoreSpec.hs**:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module MyProject.CoreSpec (spec) where

import Control.Monad.Except (runExceptT)
import Test.Hspec
  ( Spec
  , describe
  , it
  , shouldBe
  , shouldSatisfy
  )
import Test.QuickCheck (property)

import MyProject.Core
  ( createUser
  , validateAge
  , validateEmail
  )
import MyProject.Types (AppError (..), User (..))

spec :: Spec
spec = do
  describe "validateEmail" $ do
    it "accepts valid email" $ do
      result <- runExceptT $ validateEmail "test@example.com"
      result `shouldBe` Right ()
    
    it "rejects invalid email" $ do
      result <- runExceptT $ validateEmail "invalid-email"
      result `shouldSatisfy` isValidationError
  
  describe "validateAge" $ do
    it "accepts valid age" $ do
      result <- runExceptT $ validateAge 25
      result `shouldBe` Right ()
    
    it "rejects negative age" $ do
      result <- runExceptT $ validateAge (-1)
      result `shouldSatisfy` isValidationError
    
    it "rejects unrealistic age" $ do
      result <- runExceptT $ validateAge 200
      result `shouldSatisfy` isValidationError
  
  describe "createUser" $ do
    it "creates user with valid data" $ do
      result <- runExceptT $ createUser "John" "john@example.com" (Just 25)
      result `shouldSatisfy` isRight
    
    it "fails with invalid email" $ do
      result <- runExceptT $ createUser "John" "invalid" (Just 25)
      result `shouldSatisfy` isLeft

isValidationError :: Either AppError a -> Bool
isValidationError (Left (ValidationError _)) = True
isValidationError _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isLeft :: Either a b -> Bool
isLeft = not . isRight
```

### 3. QuickCheck Properties

```haskell
spec :: Spec
spec = do
  describe "QuickCheck properties" $ do
    it "validateAge rejects negative numbers" $
      property $ \n ->
        n < 0 ==>
          runExceptT (validateAge n) `shouldSatisfy` isLeft
```

### 4. Coverage Requirements

- **Overall**: > 90%
- **Critical functions**: 100%

**Check coverage**:

```bash
# Stack
stack test --coverage

# Cabal
cabal test --enable-coverage
```

---

## Build & Deployment

### 1. Build Commands

```bash
# Stack
stack build                    # Build project
stack build --fast             # Fast build (no optimization)
stack build --profile          # Build with profiling
stack clean                    # Clean build artifacts

# Cabal
cabal build                    # Build project
cabal clean                    # Clean
```

### 2. Docker Support

**Dockerfile**:

```dockerfile
# Build stage
FROM haskell:9.6.3 AS builder

WORKDIR /app

# Copy dependency files
COPY stack.yaml package.yaml stack.yaml.lock ./

# Install dependencies
RUN stack build --only-dependencies

# Copy source
COPY . .

# Build application
RUN stack build --copy-bins --local-bin-path /app/bin

# Production stage
FROM ubuntu:22.04

WORKDIR /app

# Install runtime dependencies
RUN apt-get update && \
    apt-get install -y libgmp10 netbase && \
    rm -rf /var/lib/apt/lists/*

# Copy binary from builder
COPY --from=builder /app/bin/my-project-exe /app/

# Create non-root user
RUN useradd -m -u 1000 appuser && \
    chown -R appuser:appuser /app

USER appuser

EXPOSE 8080

ENTRYPOINT ["/app/my-project-exe"]
```

### 3. Publishing to Hackage

```bash
# Generate tarball
cabal sdist

# Upload to Hackage
cabal upload dist-newstyle/sdist/my-project-0.1.0.0.tar.gz

# Upload documentation
cabal upload --documentation dist-newstyle/my-project-0.1.0.0-docs.tar.gz
```

---

## Documentation

### 1. Haddock Comments

```haskell
-- | Process a payment transaction.
--
-- This function handles the complete payment workflow including
-- validation, authorization, and confirmation.
--
-- ==== __Examples__
--
-- >>> processPayment "user-123" 1000 "USD"
-- Right (PaymentResult { ... })
--
-- >>> processPayment "user-123" (-100) "USD"
-- Left (ValidationError "Amount must be positive")
--
processPayment ::
  Text ->    -- ^ User ID
  Int ->     -- ^ Amount in cents
  Text ->    -- ^ Currency code
  IO (Either AppError PaymentResult)
processPayment userId amount currency = undefined
```

### 2. Generate Documentation

```bash
# Stack
stack haddock

# Cabal
cabal haddock

# Open docs
open .stack-work/install/.../doc/index.html
```

---

## Haskell Best Practices

See [BEST_PRACTICES.md](BEST_PRACTICES.md) for complete guide.

---

## Common Patterns

See [BEST_PRACTICES.md](BEST_PRACTICES.md).

---

## Troubleshooting

### Common Issues

**Issue**: Type error

**Solution**: Read the error message carefully, GHC provides detailed type information

---

## Complete Workflow

Follow the [AI Integration Manual Template](../templates/AI_INTEGRATION_MANUAL_TEMPLATE.md) for the complete 6-phase workflow.

---

## Additional Resources

- [Haskell.org](https://www.haskell.org/)
- [Learn You a Haskell](http://learnyouahaskell.com/)
- [Real World Haskell](http://book.realworldhaskell.org/)
- [Hoogle](https://hoogle.haskell.org/)
- [BEST_PRACTICES.md](BEST_PRACTICES.md)

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-11 | Initial Haskell manual |

---

**Maintained by**: HiveLLM Governance Team  
**License**: MIT  
**Last Review**: 2025-10-11

