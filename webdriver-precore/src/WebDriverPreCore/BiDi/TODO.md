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
- [x] recover http session after refactor
- [x] simplify Command - use value parameter :: THINK MAY NEED TO CHANGE HTTP BACK
- [NA] not worth it - mostly done finsih when split libraries - needs own types when moved to own library - decouple runner and socket from protocol
  - [X] clean up last bidi imports
- [X] specific code TODOs - TODO tree
- [X] check all exports - should use prtocol and API only
- [X] update to latest spec
- [X] unsubscribe by attributes
- [X] fallback
- [NA] IO only demo - NA wont do

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
  - [X] relook at parse http error
- [x] test => demos => test
- [x] review demoUtils
  - [x] http may not need timeLimitLog et. al. 
  - [x] delete static functions
  - [x] rerun tests
    - [x] legacy flag 
    - [x] legacy flag run
- [X] add deprecation warning
- [X] update to latest spec
- [X] fallback
- [X] demo parsing additional prop on empty return type - fallback
- [NA] - not worth it - IO only demo

## Clean up

- [X] remove unneeded instances to and fromJSON
- [X] Claude reconcile / review


## Extend testing

- [ ] chrome
  - [ ] container
  - [ ] CI
- [ ] edge
  - [ ] container
  - [ ] CI
- [ ] check if browsingContextSetViewportResetDemo runs
 - [ ] update debug config
   - [ ] move to .config folder
   - [ ] add debugConfg.hs.template (and ensure in version control)
 - [ ] update dhall.config

## rename IO Utils DemoRoot - NOT done N/A

## other

[x] runFailDemo
[X] deduplicate URL type


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

- [X] generate Haddock
  - [NA] haddock variable for versions - HADDOCK LIMITATION
  - [X] add urls to bidi
  - [X] move intro to haddock
  - [X] move demo discussion to demo
- [X] document all Fallback functions
- [X] demos index
- [X] config explained
- [X] update haddock
- [X] update readme
- [X] lookup / document dealing with different versions
- [X] check docs urls
- [X] check all licence files
- [X] redo diagram in drawIO
- [X] http diagram
- [X] change log
- [X] mention git LFS in Docs

## prep

- [ ] spell check
- [ ] check hlint and why it isn't firing as it use to
- [ ] run weeder

## CI

 - [X] get CI tests working in container
 - [X] chrome - local
 - [X] retest dev-container
   - [X] dhall
   - [X] dhall-lsp-server

## release http / bidi

- [ ] hackage
- [ ] stackage
- [ ] announce
- [ ] ping webdriver repo

--- 
delete this later

**Chrome (Chromedriver)**

*`--log-level=ALL` is optional*

*The port is set to 4444 to match the port hard coded in our test suite*

```
> pkill -f chromedriver || true && chromedriver --log-level=ALL --port=4444
```

*expect output like*

```
Starting ChromeDriver 135.0.7049.52 (9ba7e609d28c509a8ce9265c2247065d8d251173-refs/branch-heads/7049_41@{#4}) on port 4444
Only local connections are allowed.
Please see https://chromedriver.chromium.org/security-considerations for suggestions on keeping ChromeDriver safe.
ChromeDriver was started successfully on port 4444.
```

## FINAL
[X] chromedriver working
[X] rerun firefox 
[x] Claude audit
[X] spec update
[X] docs lfs git
[X] use one base container for dev-container and CI (update)
[X] devcontainer fixed
[X] docs use image
[X] once upon a time
[X] check type returned http
[ ] Claude check typos


## deferred
 - [ ] edge driver
 - [ ] chrome - CI
 - [ ] test with driver not running
 - [ ] rearrange docker - move hls install to just before dev-container

## later

- [ ] [update readme in similar style to](https://github.com/nikita-volkov/hasql)



