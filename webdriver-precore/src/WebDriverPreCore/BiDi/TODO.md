# TODO

## bidi

- [NA] split off bidi
  - [NA]  new cabal file
  - [NA]  move shared files
- [x] demos => test 
  - All 97 tests passed (220.75s)
  - All 97 tests passed (181.01s) - headless
- [x] pull logger out
- [x] rename BidiActions/Methods BiDiSocket / BiDiActions
- [x] recocover http session after refactor
- [x] simplify Command - use value parameter :: THINK MAY NEED TO CHANGE HTTP BACK
- [NA] not worth it - mostly done finsih when split libraries - needs own types when moved to own library - decouple runner and socket from protocol
  - [ ] clean up last bidi imports
- [ ] specific code TODOs - TODO tree
- [ ] check all exports - should use prtocol and API only
- [ ] update to latest spec
- [ ] remove unneeded instances to and fromJSON
- [ ] unsubscribe by attributes
- [ ] Claude reconcile / review
- [X] fallback
- [ ] IO only demo

## http

- [x] switch to local http files
  - before 
    - headed - All 22 tests passed (148.72s)
    - headless - All 22 tests passed (135.98s)
  - after
    - 3 out of 22 tests failed (117.57s) (headless)
    - All 22 tests passed (109.25s) (headed)
    - All 22 tests passed (96.09s) (headless)
  - [x] remove old constants (links and selectors)
- [x] switch to command (leave legacy)
  - [x] create Protocol
  - [x] create API
  - [x] make Command fromJson on value
  - [x] test Script with params / no params
    - [x] headed - 6 out of 23 tests failed (66.02s)
    - [x] sleep -> pause
    - [x] headed - 6 out of 23 tests failed (39.81s)
    - [x] headless - 6 out of 23 tests failed (30.14s)
    - [x] first fix
      - [x] 3 out of 23 tests failed (61.70s) - headed
      - [x] All 23 tests passed (46.59s) 
  - [x] shadowRoot - need to use parseShodowFieldname
  - [x] legacy in terms of Protocol / API
  - [x] refactor all tests in terms of API / Protocol 
  - [ ] relook at parse http error
- [x] test => demos => test
- [x] review demoUtils
  - [x] http may not need timeLimitLog et. al. 
  - [x] delete static functions
  - [x] rerun tests
    - [x] legacy flag 
    - [x] legacy flag run
- [X] add deprecation warning
- [X] update to latest spec
- [ ] remove unneeded instances to and fromJSON
- [X] fallback
- [X] demo parsing additional prop on empty return type - fallback
- [ ] IO only demo

## Extend testing

- [ ] chrome
  - [ ] container
  - [ ] CI
- [ ] edge
  - [ ] container
  - [ ] CI

## rename IO Utils DemoRoot

## other

[x] runFailDemo
[X] deduplicate URL type
[ ] test with driver not running

## change config - for CI only

- [ ] generate CI Config 
- [ ] run CI Config if present

## get rid of extended

- [X] http
  - [X] command
- [X] bidi
  - [X] command
  - [X] subscription

## update 

- [X] update bidi to latest spec
- [X] update http to latest spec

## docs

- [ ] redo diagram in drawIO
- [ ] http diagram
- [ ] document all Fallback functions
- [ ] demos index
- [ ] config explaind
- [ ] update haddock
- [ ] update readme
- [ ] lookup / document dealing with diffferent versions
- [ ] check docs urls
- [ ] check all licence files

## prep

- [ ] spell check
- [ ] check hlint and why it isn't firing as it use to
- [ ] run weeder

## CI

 - [ ] use one base container for dev-container and CI (update)
 - [ ] retest dev-container
   - [ ] dhall
   - [ ] dhall-lsp-server
 - [ ] chrome
 - [ ] edge

## release http / bidi

- [ ] hackage
- [ ] change log
- [ ] announce



