# Project Memory

## Tests

- `cabal test all` runs the test suites sequentially and, by default, stops
  after the first failing suite. Later suites (e.g. `test-effectful`) never run
  when an earlier suite fails.
- `cabal.project` sets `keep-going: True` so all test suites run even when some
  fail. Do not remove that line unless you intend to restore the
  stop-on-first-failure behavior.
- The VS Code "test all" task pipes output to `test-all.log` at the repo root.
