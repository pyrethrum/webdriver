# Compound Effect Handles vs Free Functions for WebDriver Actions in Bluefin

## Abstract

In Bluefin, there are two broad ways to expose effectful WebDriver operations to callers: **free functions** that accept an environment handle as an explicit parameter, or **capability handles** that bundle those operations as record fields (the compound effect pattern documented in Bluefin itself). This document explores what a capability-handle design would look like for the existing `HttpSessionEnv`/`BiDiEnv`-based action API, analyses the implications for callers and for the library, and argues for a hybrid approach as the most idiomatic Bluefin solution.

---

## Compound Effect Handles vs Free Functions for WebDriver Actions

### What Changing to a Capability Handle Would Look Like

Instead of free functions like:

```haskell
-- current approach
navigateTo :: (e :> es) => HttpSessionEnv e -> Text -> Eff es ()
screenshot  :: (e :> es) => HttpSessionEnv e -> Eff es Text
```

You would define a **capability handle** where the methods *are fields*:

```haskell
data WebDriverActions e = MkWebDriverActions
  { navigateTo  :: Text -> Eff e ()
  , screenshot  :: Eff e ()
  , click       :: Selector -> Eff e ()
  , pause       :: Eff e ()
  }
  deriving (Generic)
  deriving (Handle) via OneWayCoercibleHandle WebDriverActions

instance (e :> es) => OneWayCoercible (WebDriverActions e) (WebDriverActions es) where
  oneWayCoercibleImpl = gOneWayCoercible
```

And construction would capture the env:

```haskell
mkHttpSessionActions :: HttpSessionEnv e -> IOE e -> WebDriverActions e
mkHttpSessionActions sess io = MkWebDriverActions
  { navigateTo  = \url -> runHttpCommand sess (HC.navigateTo sess.httpSession url)
  , screenshot  = runHttpCommand sess HC.screenshot
  , pause       = effIO io $ threadDelay (let MkTimeout us = sess.pauseDuration in us)
  }
```

---

### The Key Technical Issue: `Eff e` vs `Eff es`

This is the **critical difference**. The Bluefin compound effect pattern works cleanly for handles that wrap `IOE e` directly. But action fields have type `Text -> Eff e ()`, which bakes in a *fixed* `e`. Callers working in `Eff es ()` (where `e :> es`) would face a mismatch:

```haskell
useActions :: (e :> es) => WebDriverActions e -> Eff es ()
useActions actions = do
  actions.navigateTo "https://example.com"   -- Eff e () ... in Eff es () context
  --                                            ^^^^^^^^ MISMATCH unless e ~ es
```

This is resolved by passing the `WebDriverActions e` handle itself through `useImplIn`, so that the `e` in the field types *is* the widened scope at call sites.

---

### Implications

| Concern | Free Functions (current) | Capability Handle |
|---|---|---|
| Effect widening | automatic via `:>` constraint | requires routing through `useImplIn` |
| Adding a new action | add a function | add a field + update constructor |
| Mocking / testing | need a separate mock env | **swap the whole handle** — very clean |
| Multiple backends (HTTP vs BiDi) | `viaSession`-style adapters | one type, two smart constructors |
| Type inference | excellent | can degrade with rank-2 fields |
| Partial capability sets | hard | easy — give callers only the fields they need |
| Haskell ecosystem familiarity | standard | less common, closer to OO style |

---

### Which Is More Idiomatic Bluefin?

The idiomatic choice depends on **what varies**:

**Use free functions (current approach) when:**
- The effect type is fixed (`HttpSessionEnv`, `BiDiEnv`)
- Callers always know which backend they're using
- Maximum type inference is a priority

**Use capability handles when:**
- You want **backend polymorphism** — the same test code runs against HTTP *or* BiDi without changes
- You want **testability** — inject a mock `WebDriverActions` in unit tests without spinning up a real driver
- The capability set is a meaningful abstraction boundary

The existing `viaSession` pattern is already a partial step toward capability handles — it constructs a `Runner` function that closes over a session. A full capability handle is the logical completion of that idea.

---

### Practical Recommendation: A Hybrid

The most idiomatic Bluefin solution for this codebase is a **backend-polymorphic capability handle**:

```haskell
-- A capability handle that abstracts over backend
data WebDriverSess e = MkWebDriverSess
  { navigate   :: Text -> Eff e ()
  , getSource  :: Eff e Text
  , pause      :: Eff e ()
  -- ... etc
  }
  deriving (Generic)
  deriving (Handle) via OneWayCoercibleHandle WebDriverSess

-- Constructed from either backend
fromHttpSession :: HttpSessionEnv e -> WebDriverSess e
fromBiDi        :: BiDiEnv e        -> WebDriverSess e
```

User/test code would only ever see `WebDriverSess e` — never the backend-specific env types. `useImplIn` provides effect widening, and swapping backends is a single constructor change at the top level. This is precisely the pattern Bluefin's compound effect machinery was designed for.

*Note: this would only work for functions that have the same API in both BiDi and Http*
