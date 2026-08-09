---
name: haskell-coding-style
description: 'Haskell coding style guidelines for this project. Use when: writing or reviewing Haskell code, applying project conventions, checking style, pattern matching on ADTs or Maybe/Either.'
---

# Haskell Coding Style

## Core Project Rules

## Code Style
- Use 2-space indentation
- Prefer explicit type signatures

## Patterns to Follow
- Prefer `(&)` for pipeline-style operations
- Use record syntax with OverloadedRecordDot
- Use `maybe`, `either`, `fromMaybe` rather than explicit case statements for `Maybe` and `Either`
- Use `LambdaCase`
- Prefer `f . g . h $ x` over `f $ g $ h $ x`

## Avoid
- String type (prefer Text)
- Nested if-then-else (use case or guards)
- Trailing closing parentheses (prefer `$`)

## Prefer LambdaCase

Use `\case` instead of a named lambda when pattern-matching on the sole argument.

for data type:

```haskell
data Foo = Bar Int | Baz
```

**Avoid:**
```haskell
handleFoo foo = case foo of
  Bar n -> n + 1
  Baz   -> 0

map (\x -> case x of
  Bar n -> n + 1
  Baz   -> 0) xs
```

**Prefer:**
```haskell
handleFoo = \case
  Bar n -> n + 1
  Baz   -> 0

map (\case
  Bar n -> n + 1
  Baz   -> 0) xs
```

Requires `{-# LANGUAGE LambdaCase #-}` (already enabled project-wide).

## Prefer `maybe`, `fromMaybe`, and `either` over Pattern Matching

Use the combinator forms instead of explicit `case` on `Maybe` and `Either`.

**Avoid:**
```haskell
case mValue of
  Nothing -> 0
  Just n  -> n + 1

case mValue of
  Nothing -> defaultVal
  Just x  -> x

case result of
  Left err -> handleError err
  Right val -> handleSuccess val
```

**Prefer:**
```haskell
maybe 0 (+ 1) mValue

fromMaybe defaultVal mValue

either handleError handleSuccess result
```

### Use `(&)` when the positive branch is large

When the handler is too large for point-free style, pipe the target into the combinator with `(&)` rather than naming a lambda parameter:

**Avoid:**
```haskell
case mConfig of
  Nothing  -> defaultConfig
  Just cfg ->
    cfg { port = 8080
        , host = "localhost"
        , debug = True
        }

case eResult of
  Left err ->
    logError err *> pure fallback
  Right val ->
    process val >>= save >>= notify
```

**Prefer:**
```haskell
mConfig & maybe defaultConfig
  (\cfg -> cfg { port = 8080
               , host = "localhost"
               , debug = True
               })

eResult & either
  (\err -> logError err *> pure fallback)
  (\val -> process val >>= save >>= notify)
```

## Prefer Point-Free Style

Omit the final argument when it can be dropped cleanly, especially in short functions and `where` bindings.

**Avoid:**
```haskell
isValid x = check x && verify x

labelOf x = getName x <> ": " <> getValue x

processAll xs = map transform xs

toInts xs = mapMaybe parseNum xs
```

**Prefer:**
```haskell
isValid = check <> verify   -- or: (&&) <$> check <*> verify

labelOf = getName <> (": " <>) . getValue  -- or compose explicitly:
labelOf = \x -> getName x <> ": " <> getValue x  -- if composition hurts clarity

processAll = map transform

toInts = mapMaybe parseNum
```

Use `(.)` and `($)` to chain transforms without naming intermediate values:

```haskell
-- Avoid
render x = toText (format (normalise x))

-- Prefer
render = toText . format . normalise
```

**Limit**: don't sacrifice clarity. If point-free requires `(.).(.)` or `flip` gymnastics, name the argument instead.

## Avoid List Comprehensions

Prefer `traverse`, `for`, `map`, `filter`, and `replicateM` over list comprehensions.

**Avoid:**
```haskell
[f x | x <- xs]

[x | x <- xs, predicate x]

sequence [mkChild i | i <- [1 .. count]]
```

**Prefer:**
```haskell
f <$> xs

filter predicate xs

traverse mkChild [1 .. count]
```

## Prefer Functor/Applicative/Monad Combinators over Explicit Recursion

Prefer the standard library combinators (`map`, `filter`, `filterM`, `traverse`, `foldM`, `sequence`, `<$>`, `<*>`, `>>=`, `join`, `maybe`, ...) over hand-rolled recursion or manual `case`/`do`-loop scaffolding whenever they express the intent. Recursion should be a last resort for genuinely recursive structures (e.g. tree walks), not for ordinary list/optional processing.

**Avoid:**
```haskell
filterByText MkLocParams{getElementText} keep = fmap join . traverse go
  where
    go node = case nodeToSharedRef node of
      Nothing -> pure []
      Just ref -> do
        t <- getElementText ref
        pure $ if keep t then [node] else []
```

**Prefer** — combine `filterM` (monadic filter over the list) with `maybe` and `<$>`:
```haskell
filterByText MkLocParams{getElementText} keep = filterM keep'
  where
    keep' :: BiDiP.NodeRemoteValue -> m Bool
    keep' = maybe (pure False) ((keep <$>) . getElementText) . nodeToSharedRef
```

Other examples:
```haskell
-- replace manual foldl recursion with foldM / foldl'
total = foldl' (+) 0 xs

-- replace mapM/sequence scaffolding with traverse
results = traverse readFile paths

-- replace nested case on Maybe with maybe
firstOrNil = maybe [] pure
```

**Limit**: don't force a combinator if it obscures the logic — a short, obvious recursive helper is preferable to `(.).(.)` gymnastics (see the point-free limit above).

## Prefer `<$>` over `fmap`, `map`, and `second`

Use the infix `<$>` operator instead of `fmap`, `map` (on functors other than lists where clarity permits), and `second`.

**Avoid:**
```haskell
fmap f mValue

map f someList

second f pair
```

**Prefer:**
```haskell
f <$> mValue

f <$> someList

f <$> pair   -- works for tuples via the Functor instance for (,) a
```

## Prefer `$` over Trailing Parentheses

Use `$` to eliminate closing parentheses at the end of an expression.

**Avoid:**
```haskell
foo (bar (baz x))

when condition (doSomething arg)

liftIO (putStrLn "hello")
```

**Prefer:**
```haskell
foo $ bar $ baz x

when condition $ doSomething arg

liftIO $ putStrLn "hello"
```

**Limit**: don't use `$` when the argument is part of a larger expression that needs grouping, or when it would reduce clarity (e.g. inside operator sections or infix chains).

## Prefer `.` + Single `$` over Chained `$`

When applying a pipeline of functions to an argument, compose the functions with `.` and use a single `$` at the end rather than chaining `$`.

**Avoid:**
```haskell
foo $ bar $ baz $ qux param

toText $ format $ normalise x
```

**Prefer:**
```haskell
foo . bar . baz $ qux param

toText . format $ normalise x
```

## No Constructor/Type Punning

Type names and their constructors must have distinct names. For single-constructor types use the `Mk` prefix on the constructor.

**Avoid:**
```haskell
data Config = Config { host :: Text, port :: Int }

data RequestId = RequestId Text
```

**Prefer:**
```haskell
data Config = MkConfig { host :: Text, port :: Int }

data RequestId = MkRequestId Text
```

For multi-constructor ADTs each constructor already has a distinct name, so no prefix is needed:

```haskell
data Locator
  = CSS  { value :: Text }
  | XPath { value :: Text }
  | AllElms
```

## Tasty Test and Group Names

Tasty's `-p/--pattern` shorthand treats the name as a plain substring only when it contains **solely** letters, digits, and the characters `. _ - ` (period, underscore, hyphen, space). Any other character — `:`, `(`, `)`, `=`, `,`, `;`, `+`, `/`, `'`, `[`, `]`, `&`, `|`, `>` — causes the argument to be parsed as an awk expression, which fails on natural-language text and breaks VS Code's test-explorer run-single-test integration.

**Rule**: keep `testGroup` / `testCase` / `test` / helper names to letters, digits, spaces, hyphens, underscores, and periods only.

**Avoid:**
```haskell
testGroup "Rule 1 (display=none on element itself)" [...]
test "locate (singleton): resolves hidden/visible ambiguity" $ ...
chkAll "AND: input_ &&& elmClass text-input" loc chk
```

**Prefer:**
```haskell
testGroup "Rule 1 - display none on element itself" [...]
test "locate singleton - resolves hidden-visible ambiguity" $ ...
chkAll "AND - input_ and elmClass text-input" loc chk
```
