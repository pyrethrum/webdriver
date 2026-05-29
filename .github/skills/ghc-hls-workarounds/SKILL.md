---
name: ghc-hls-workarounds
description: 'GHC / HLS workarounds for known bugs and panics. Use when: diagnosing HLS errors, investigating unexpected GHC panics, applying workarounds for type-family-related compilation issues.'
---

# GHC / HLS Workarounds

## `expectJust` Panic in `GHC.Tc.Instance.Family` (HLS only)

### Symptom

HLS reports a compilation error like:

```
panic! (the 'impossible' happened)
  GHC version 9.14.1:
	expectJust

Call stack:
    CallStack (from HasCallStack):
      pprPanic, called at compiler/GHC/Data/Maybe.hs:77:19 in ghc-9.14.1-c6c3:GHC.Data.Maybe
      expectJustError, called at compiler/GHC/Data/Maybe.hs:74:24 in ghc-9.14.1-c6c3:GHC.Data.Maybe
      expectJust, called at compiler/GHC/Tc/Instance/Family.hs:409:30 in ghc-9.14.1-c6c3:GHC.Tc.Instance.Family
```

This appears in files using `effectful`-style type families — particularly `DispatchOf` and the `(:>)` constraint.

### Affected Versions

- **GHC 9.14.1** — confirmed affected
- `cabal build` is **not** affected; this is a purely an HLS typechecking session issue

### Root Cause

`getFamInsts` at `Family.hs:409` calls `expectJust` on `lookupModuleEnv (eps_mod_fam_inst_env eps) mod`. It panics when HLS's GHC session hasn't loaded the module's family instances into the External Package State (EPS) after loading its interface file.

### GHC History

| GHC MR | Description |
|--------|-------------|
| !14402 | Removed `hptAllFamInstances` usage — introduced this panic |
| !14763 | Reverted the removal |
| !14924 | Backported revert to GHC 9.14.1 |

The revert was backported, but HLS can still trigger the remaining `expectJust` under certain conditions. **Note:** GHC bug #27214 is a *different* unrelated hs-boot/TH linking bug — do not confuse them.

### Workaround

Add **explicit type signatures** to:

- Local `where` and `let` bindings that involve type families
- `do`-notation `<-` bindings whose result type involves type families

**Examples:**

```haskell
-- In a where clause
go :: Eff [SomeEffect, IOE] ()
go = ...

-- In do notation (inline annotation)
(config :: Config) <- liftIO loadConfig

-- In a let binding
let defOpts :: L.HttpLocateOpts
    defOpts = defaultOpts
```

**Why it helps:** Explicit signatures reduce the type inference scope, avoiding the family-instance consistency-check codepath that triggers the panic in HLS.

### Indirect Triggers

The panic can also appear in a *dependency* rather than the file being edited. For example, if you have eval comments (`-- >>>`) that transitively import a file with `(:>)` constraints (e.g. `Common/Runner.hs`), the panic will be reported against that dependency, not the file open in the editor.

Apply the same workaround (explicit signatures) in the dependency file.
