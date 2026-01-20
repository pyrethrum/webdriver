
I am doing a major reorg of this library with a view to publishing new libraries. As an interim step I want to extend reorg webdriver-precore with private libraries for integrated initial development.

## updated project structure

in the interim i want a library structure as follows:

- exising libraries
  - library (webdriver-precore)
  - utils-internal
  - test-suite test
  - test-server
- new internal libraries (to be added to webdriver-precore cabal file)
  - webdriver-precore-extended
  - webdriver-precore-http-runner-base
  - webdriver-precore-http-runner
  - webdriver-precore-bidi-runner-base
  - webdriver-precore-bidi-runner
  - webdriver-precore-driver-control
  - webdriver-effectful
  - webdriver-rio-poc
  - webdriver-bluefin-poc
- test suites for all new libraries 

## purpose of libraries
### webdriver-precore-extended

1. re-exports webdriver-precore (the API and protocol modules under Base module)
2. new extended funtions
   1. these will extend base with convenience functions that will not have any typeclass, monadic helpers (multi param very basic functions)

### webdriver-precore-http-runner-base

provides an http runner (port of existing runner) but interface will be JSON based (will still have HTTP response type but not depend on webdriver-precore library and types in any way (use JSON Values instead).

### webdriver-precore-http-runner

takes webdriver-precore-http-runner-base and  adds a dependency to webdriver-precore adds types so JSON values (in http-runner-base) become typed Commands  webdriver-precore 

### webdriver-precore-bidi-runner-base

as for http base but implments the runner in terms of a socket

### webdriver-precore-bidi-runner

as for http base but implments the runner in terms of a socket

### webdriver-precore-driver-control

a module that enables starting / terminating drivers and downloading  and installing drivers from the web

### webdriver-effectful

a wraper around  webdriver-precore-extended maps all basic webdriver-precore-extended functions to effectful

### webdriver-rio-poc

same as webdriver-effectful but uses RIO and RIO idioms - may not be completed this is really to stress test the design of webdriver-precore-extended and prove it can be adapted to multiple architectures 

### webdriver-bluefin-poc

same as webdriver-effectful but uses bluefin and blufin idiomss - may not be completed this is really to stress test the design of webdriver-precore-extended and prove it can be adapted to multiple architectures 
 
## required initialisation

Given the above, we need need to inialise all these projects. Do not add any extra modules not listed below - they will come later.

