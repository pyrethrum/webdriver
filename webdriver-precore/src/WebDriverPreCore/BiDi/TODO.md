# TODO

## bidi
- [NA] split off bidi
  - [NA]  new cabal file
  - [NA]  move shared files
- [x] demos => test 
  - All 97 tests passed (220.75s)
  - All 97 tests passed (181.01s) - headless
- [x] pull logger out
- [ ] rename BidiActions/Methods BiDiSocket / BiDiActions
- [ ] specific code TODOs - TODO tree
- [ ] check all exports - should use prtocol and API only
- [ ] update to latest spec
- [ ] remove unneeded instances to and fromJSON
- [ ] Claude reconcile / review

## http
- [ ] test => demos => test
- [x] switch to local http files
  - before 
    - headed - All 22 tests passed (148.72s)
    - headless - All 22 tests passed (135.98s)
  - after
    - 3 out of 22 tests failed (117.57s) (headless)
    - All 22 tests passed (109.25s) (headed)
    - All 22 tests passed (96.09s) (headless)
  - [x] remove old constants (links and selectors)
- [ ] switch to command (leave legacy)
- [ ] update to latest spec
- [ ] switch to protocol and API - keep legacy
- [ ] remove unneeded instances to and fromJSON
- [ ] review demoUtils
  - [ ] http may not need timeLimitLog et. al. 
  - [ ] delete static functions

## update 
  - update bidi to latest spec
  - update http to latest spec

## docs - 
- [ ] demos index
- [ ] config explaind
- [ ] update haddock
- [ ] update readme
- [ ] lookup / document dealing with diffferent versions
- [ ] check docs urls
- [ ] check all licence files

## prep
- [ ] check hlint and why it isn't firing as it use to
- [ ] run weeder

## CI
 - [ ] use one base container for dev-container and CI (update)
 - [ ] retest dev-container

## Extend testing
- [ ] chrome
  - [ ] container
  - [ ] CI
- [ ] edge
  - [ ] container
  - [ ] CI

## release http
- [ ] hackage
- [ ] change log
- [ ] announce

## release bidi
- [ ] hackage
- [ ] change log
- [ ] announce

