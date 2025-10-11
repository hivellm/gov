# Haskell Best Practices Guide

**Version**: 1.0.0  
**Last Updated**: 2025-10-11  
**Target Audience**: AI Agents & Developers

---

## Table of Contents

1. [Haskell Idioms](#haskell-idioms)
2. [Anti-Patterns](#anti-patterns)
3. [Type System Best Practices](#type-system-best-practices)
4. [Performance Optimization](#performance-optimization)
5. [Error Handling](#error-handling)
6. [Monad Patterns](#monad-patterns)
7. [Laziness Management](#laziness-management)
8. [Common Gotchas](#common-gotchas)

---

## Haskell Idioms

### 1. Use Newtype Wrappers

**✅ Good**:
```haskell
newtype UserId = UserId { unUserId :: Int }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype Email = Email { unEmail :: Text }
  deriving (Eq, Show)

-- Type-safe function
createUser :: UserId -> Email -> User
createUser userId email = User userId email
```

**❌ Bad**:
```haskell
-- Primitive obsession
createUser :: Int -> Text -> User
createUser userId email = User userId email

-- Easy to mix up parameters!
createUser email userId  -- Wrong order, no compiler error
```

### 2. Smart Constructors

**✅ Good**:
```haskell
module MyProject.Types
  ( Email  -- Export type but not constructor
  , mkEmail  -- Export smart constructor
  , unEmail
  ) where

newtype Email = Email { unEmail :: Text }
  deriving (Eq, Show)

-- Smart constructor with validation
mkEmail :: Text -> Either Text Email
mkEmail email
  | "@" `T.isInfixOf` email = Right (Email email)
  | otherwise = Left "Invalid email format"
```

**❌ Bad**:
```haskell
-- Direct constructor allows invalid data
let email = Email "not-an-email"  -- No validation!
```

### 3. Use Algebraic Data Types

**✅ Good**:
```haskell
data Result a
  = Success a
  | Error Text
  | NotFound
  deriving (Eq, Show)

data PaymentStatus
  = Pending
  | Processing
  | Completed { completedAt :: UTCTime }
  | Failed { reason :: Text }
  deriving (Eq, Show)

-- Pattern matching is exhaustive
handleResult :: Result User -> Text
handleResult (Success user) = "User: " <> userName user
handleResult (Error msg) = "Error: " <> msg
handleResult NotFound = "Not found"
```

### 4. Function Composition

**✅ Good**:
```haskell
-- Point-free style
processUsers :: [User] -> [Text]
processUsers = map userName . filter isActive . sortBy compareAge

-- Compose with (.)
formatUser :: User -> Text
formatUser = toUpper . userName

-- Apply with ($)
result = processData $ filterInvalid $ parseInput input
```

**❌ Bad**:
```haskell
processUsers users =
  let sorted = sortBy compareAge users
      filtered = filter isActive sorted
      mapped = map userName filtered
  in mapped
```

### 5. Use Applicative Style

**✅ Good**:
```haskell
-- Applicative validation
validateUser :: Text -> Text -> Int -> Either [Text] User
validateUser name email age =
  User <$> validateName name
       <*> validateEmail email
       <*> validateAge age

-- Parse applicative
data Config = Config
  { configPort :: Int
  , configHost :: Text
  , configDb :: Text
  }

parseConfig :: Parser Config
parseConfig =
  Config <$> option auto (long "port")
         <*> strOption (long "host")
         <*> strOption (long "db")
```

### 6. Leverage Type Classes

**✅ Good**:
```haskell
-- Define custom type class
class Serializable a where
  serialize :: a -> ByteString
  deserialize :: ByteString -> Either Text a

instance Serializable User where
  serialize = encode
  deserialize = eitherDecode

-- Use type class constraints
saveToFile :: (Serializable a) => FilePath -> a -> IO ()
saveToFile path x = BS.writeFile path (serialize x)
```

### 7. Use do-notation Appropriately

**✅ Good**:
```haskell
-- Complex monadic sequence
processRequest :: IO (Either Text User)
processRequest = runExceptT $ do
  userId <- ExceptT $ validateId requestId
  user <- ExceptT $ getUser userId
  ExceptT $ checkPermissions user
  liftIO $ logAccess user
  pure user

-- Simple composition doesn't need do
getUserName :: UserId -> IO Text
getUserName = fmap userName . getUser
```

**❌ Bad**:
```haskell
-- Unnecessary do for single operation
getUserName userId = do
  user <- getUser userId
  return (userName user)
```

### 8. Pattern Guards

**✅ Good**:
```haskell
classifyAge :: Int -> Text
classifyAge age
  | age < 0 = "Invalid"
  | age < 13 = "Child"
  | age < 20 = "Teenager"
  | age < 65 = "Adult"
  | otherwise = "Senior"

-- With pattern matching
processUser :: Maybe User -> Text
processUser (Just user)
  | isAdmin user = "Admin: " <> userName user
  | isActive user = "Active: " <> userName user
  | otherwise = "Inactive: " <> userName user
processUser Nothing = "No user"
```

### 9. Record Syntax

**✅ Good**:
```haskell
data User = User
  { userId :: UserId
  , userName :: Text
  , userEmail :: Email
  , userAge :: Maybe Int
  } deriving (Eq, Show)

-- Record update
updateUserName :: Text -> User -> User
updateUserName newName user = user { userName = newName }

-- Pattern matching with records
greetUser :: User -> Text
greetUser User{userName, userAge} =
  "Hello " <> userName <> " (age: " <> showAge userAge <> ")"
```

### 10. Lens for Complex Updates

**✅ Good**:
```haskell
import Control.Lens

data Address = Address
  { _street :: Text
  , _city :: Text
  } deriving (Eq, Show)

data User = User
  { _name :: Text
  , _address :: Address
  } deriving (Eq, Show)

makeLenses ''Address
makeLenses ''User

-- Deep update
updateCity :: Text -> User -> User
updateCity newCity = address . city .~ newCity

-- View
getCity :: User -> Text
getCity = view (address . city)
```

---

## Anti-Patterns

### 1. Partial Functions

**❌ Bad**:
```haskell
head :: [a] -> a  -- Crashes on empty list
tail :: [a] -> [a]  -- Crashes on empty list
fromJust :: Maybe a -> a  -- Crashes on Nothing
```

**✅ Good**:
```haskell
-- Use safe alternatives
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- Or pattern matching
processFirst :: [a] -> (a -> b) -> Maybe b
processFirst [] _ = Nothing
processFirst (x:_) f = Just (f x)
```

### 2. String Instead of Text

**❌ Bad**:
```haskell
userName :: String  -- Linked list of Char, inefficient
```

**✅ Good**:
```haskell
import Data.Text (Text)
import qualified Data.Text as T

userName :: Text  -- Efficient UTF-16 text
```

### 3. Lazy IO

**❌ Bad**:
```haskell
-- Lazy IO can leak resources
readData :: FilePath -> IO String
readData = readFile  -- File handle may not close promptly
```

**✅ Good**:
```haskell
import qualified Data.Text.IO as TIO

-- Strict IO with explicit resource management
readData :: FilePath -> IO Text
readData path = TIO.readFile path

-- Or use bracket for safety
withFileContent :: FilePath -> (Text -> IO a) -> IO a
withFileContent path action =
  bracket (openFile path ReadMode) hClose $ \handle ->
    TIO.hGetContents handle >>= action
```

### 4. Excessive Type Classes

**❌ Bad**:
```haskell
-- Overly general, hard to understand
class MyClass a where
  process :: a -> a
  transform :: a -> b -> c
  validate :: a -> Bool
  -- Too many methods, unclear purpose
```

**✅ Good**:
```haskell
-- Focused, single responsibility
class Validatable a where
  validate :: a -> Either Text a

class Transformable a b where
  transform :: a -> b
```

### 5. Not Using Strictness When Needed

**❌ Bad**:
```haskell
-- Accumulator builds thunks
sumList :: [Int] -> Int
sumList = go 0
  where
    go acc [] = acc
    go acc (x:xs) = go (acc + x) xs  -- Lazy accumulation
```

**✅ Good**:
```haskell
-- Strict accumulator with BangPatterns
{-# LANGUAGE BangPatterns #-}

sumList :: [Int] -> Int
sumList = go 0
  where
    go !acc [] = acc  -- Force evaluation
    go !acc (x:xs) = go (acc + x) xs

-- Or use foldl'
sumList :: [Int] -> Int
sumList = foldl' (+) 0
```

### 6. Ignoring Warnings

**❌ Bad**:
```haskell
-- Unused variable warning ignored
process x y = x + 10  -- y is unused
```

**✅ Good**:
```haskell
-- Prefix with _ to indicate intentionally unused
process x _y = x + 10
```

---

## Type System Best Practices

### 1. Phantom Types

**✅ Good**:
```haskell
-- Phantom type parameter for compile-time safety
newtype Id a = Id { unId :: Int }
  deriving (Eq, Show)

type UserId = Id User
type PostId = Id Post

-- Can't mix up IDs!
getUser :: UserId -> IO User
getPost :: PostId -> IO Post
```

### 2. GADTs for Type Safety

**✅ Good**:
```haskell
{-# LANGUAGE GADTs #-}

data Expr a where
  ILit :: Int -> Expr Int
  BLit :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Eq :: Expr Int -> Expr Int -> Expr Bool

eval :: Expr a -> a
eval (ILit n) = n
eval (BLit b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Eq e1 e2) = eval e1 == eval e2
```

### 3. Type Families

**✅ Good**:
```haskell
{-# LANGUAGE TypeFamilies #-}

class Collection c where
  type Elem c
  empty :: c
  insert :: Elem c -> c -> c
  toList :: c -> [Elem c]

instance Collection [a] where
  type Elem [a] = a
  empty = []
  insert = (:)
  toList = id
```

### 4. Functional Dependencies

**✅ Good**:
```haskell
{-# LANGUAGE FunctionalDependencies #-}

class Convertible a b | a -> b where
  convert :: a -> b

instance Convertible Int String where
  convert = show

instance Convertible String Int where
  convert = read
```

---

## Performance Optimization

### 1. Use Strict Data Structures

**✅ Good**:
```haskell
{-# LANGUAGE StrictData #-}

-- All fields strict by default
data User = User
  { userId :: Int
  , userName :: Text
  }

-- Or explicit strictness
data Config = Config
  { !port :: Int
  , !host :: Text
  }
```

### 2. Unboxed Types

**✅ Good**:
```haskell
{-# LANGUAGE UnboxedTuples #-}

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

-- Use unboxed vectors for primitive types
numbers :: Vector Int
numbers = V.fromList [1..1000000]
```

### 3. Stream Fusion

**✅ Good**:
```haskell
import qualified Data.Vector as V

-- Vector operations fuse together
process :: V.Vector Int -> V.Vector Int
process = V.map (*2) . V.filter even . V.map (+1)
-- Compiles to single loop, no intermediate vectors
```

### 4. Memoization

**✅ Good**:
```haskell
import Data.MemoTrie

-- Memoized Fibonacci
fib :: Int -> Integer
fib = memo fib'
  where
    fib' 0 = 0
    fib' 1 = 1
    fib' n = fib (n-1) + fib (n-2)
```

---

## Error Handling

### 1. Use ExceptT for Error Handling

**✅ Good**:
```haskell
import Control.Monad.Except

type AppM = ExceptT AppError IO

getUser :: UserId -> AppM User
getUser userId = do
  maybeUser <- liftIO $ queryUser userId
  case maybeUser of
    Nothing -> throwError $ NotFound "User not found"
    Just user -> pure user

processRequest :: AppM Response
processRequest = do
  user <- getUser userId
  posts <- getPosts user
  pure $ Response user posts
```

### 2. Validation with Validation Type

**✅ Good**:
```haskell
import Data.Validation

type ValidationResult = Validation [Text]

validateUser :: Text -> Text -> Int -> ValidationResult User
validateUser name email age =
  User <$> validateName name
       <*> validateEmail email
       <*> validateAge age

validateName :: Text -> ValidationResult Text
validateName name
  | T.null name = Failure ["Name cannot be empty"]
  | T.length name < 2 = Failure ["Name too short"]
  | otherwise = Success name
```

### 3. Either for Short-Circuiting

**✅ Good**:
```haskell
processData :: Text -> Either Text Result
processData input = do
  parsed <- parseInput input
  validated <- validateData parsed
  result <- compute validated
  pure result
-- Stops at first error
```

---

## Monad Patterns

### 1. ReaderT for Configuration

**✅ Good**:
```haskell
import Control.Monad.Reader

data Config = Config
  { configDb :: ConnectionPool
  , configLogger :: Logger
  }

type App = ReaderT Config IO

getUser :: UserId -> App User
getUser userId = do
  pool <- asks configDb
  logger <- asks configLogger
  liftIO $ do
    logInfo logger "Fetching user"
    queryUser pool userId
```

### 2. StateT for Stateful Computations

**✅ Good**:
```haskell
import Control.Monad.State

type GameState = State GameWorld

updatePlayer :: Player -> GameState ()
updatePlayer player = modify $ \world ->
  world { worldPlayers = Map.insert (playerId player) player (worldPlayers world) }
```

### 3. Monad Transformers Stack

**✅ Good**:
```haskell
type App =
  ReaderT Config       -- Configuration
    (StateT AppState   -- Mutable state
      (ExceptT AppError  -- Error handling
        IO))           -- IO operations

runApp :: Config -> AppState -> App a -> IO (Either AppError (a, AppState))
runApp config state app =
  runExceptT $ runStateT (runReaderT app config) state
```

---

## Laziness Management

### 1. Force Evaluation When Needed

**✅ Good**:
```haskell
import Control.DeepSeq

-- Deep evaluation
processAndForce :: [a] -> [b]
processAndForce xs =
  let result = map expensiveFunction xs
  in force result  -- Fully evaluate before returning

-- Or use strict functions
sumStrict :: [Int] -> Int
sumStrict = foldl' (+) 0  -- foldl' is strict
```

### 2. Avoid Space Leaks

**✅ Good**:
```haskell
-- Mean with strict accumulation
mean :: [Double] -> Double
mean xs = sum' / count'
  where
    (sum', count') = foldl' step (0, 0) xs
    step (!s, !c) x = (s + x, c + 1)
```

---

## Common Gotchas

### 1. Monomorphism Restriction

```haskell
-- May not generalize
x = show . read  -- What type?

-- Be explicit
x :: String -> String
x = show . read
```

### 2. Lazy Fields in Records

```haskell
-- Lazy by default
data User = User
  { userId :: Int  -- Lazy!
  , userName :: Text  -- Lazy!
  }

-- Use StrictData or bang patterns
{-# LANGUAGE StrictData #-}
```

### 3. List Concatenation

```haskell
-- Inefficient: O(n) for each append
concat = foldl (++) []  -- Slow!

-- Better: use foldr or DList
concat = foldr (++) []  -- Better
```

---

## Additional Resources

- [Haskell Programming from First Principles](http://haskellbook.com/)
- [What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask/)
- [Haskell Style Guide](https://kowainik.github.io/posts/2019-02-06-style-guide)
- [Type Classes](https://typeclasses.com/)

---

**Version**: 1.0.0  
**Maintained by**: HiveLLM Governance Team  
**Last Updated**: 2025-10-11

