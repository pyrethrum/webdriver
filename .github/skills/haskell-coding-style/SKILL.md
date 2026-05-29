---
name: haskell-coding-style
description: 'Haskell coding style guidelines for this project. Use when: writing or reviewing Haskell code, applying project conventions, checking style, pattern matching on ADTs or Maybe/Either.'
---

# Haskell Coding Style

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
