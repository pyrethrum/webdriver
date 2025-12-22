<!-- # webdriver-precore-??.??.??.?? (????-??-??) - Unreleased -->

## Deprecations / Breaking Changes

### Updating from Deprecated 
- see Runner - main change is instead of calling the parser property on the HtttpSpec type, the user will extract the body value of the http response JSON and call 'parseJSON' on the result. See `fromBodyValue` in `SpecDefinition` for AN EXAMPLE

## Bug Fixes
- session status ~ change in data type and implementation to match API (previously this was incorrectly returning ready)

- update spec definitions

## New Features
- Full BiDi API and protocol

For examples as to how these changes affect a client implementation see [the test directory readme](https://github.com/pyrethrum/webdriver/tree/main/webdriver-precore/test#readme) and [changLog](https://github.com/pyrethrum/webdriver/blob/main/webdriver-precore/test/ChangeLog.md)

# webdriver-precore-0.1.0.2 (2025-05-17)

Fix Hackage build failure (ghc-9.8.4)

# webdriver-precore-0.1.0.1 (2025-04-28)

README update - Stackage badge

# webdriver-precore-0.1.0.0 (2025-04-27)

Downgrade `cabal-version:` from  `3.12` => `3.8` for Stackage compatibility

# webdriver-precore-0.0.0.4 (2025-04-21)

Fix another typo (bad repo examples link)

# webdriver-precore-0.0.0.3 (2025-04-21)

A few documentation typos an bump version of Vector dependencies

# webdriver-precore-0.0.0.2 (2025-04-21)

The initial release of this library.

See [README](README.md) for details