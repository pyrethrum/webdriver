
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

### webdriver-precore-exception

a exception handling and conversion functions

### webdriver-precore-test-resources

see below

### webdriver-effectful

a wraper around  webdriver-precore-extended maps all basic webdriver-precore-extended functions to effectful

### webdriver-rio-poc

same as webdriver-effectful but uses RIO and RIO idioms - may not be completed this is really to stress test the design of webdriver-precore-extended and prove it can be adapted to multiple architectures 

### webdriver-bluefin-poc

same as webdriver-effectful but uses bluefin and blufin idiomss - may not be completed this is really to stress test the design of webdriver-precore-extended and prove it can be adapted to multiple architectures 
 
## required initialisation

Given the above, we need need to inialise all these projects. Do not add any extra modules requested below listed below - they will come later.

- all libraries  should depend on:
  - text
  - unliftio
  - base
- new internal libraries (to be added to webdriver-precore cabal file)
  - webdriver-precore-extended
    - depends on webdriver-precore
    - reexport under Base (API * 2, Protocol * 2)
    - add no other modules
  - webdriver-precore-exception
    - imports webdriver-precore
    - single module for exception handling and conversion (see below)
  
  - webdriver-precore-http-runner-base
    - copy across HTTP response type
    - decouple from webdriver-precore by replacing command with a JSON Value and moving exception parsing into webdriver-precore-exception
  - webdriver-precore-http-runner
    - should be a very this wrapper depending webdriver-precore, precore-http-runner-base and webdriver-precore-exception
    - should have the same functionality as the http runner now in the test suite
    - migrate Http runner not the (deprecated module) from test suite

  - in the same vein as the http runner create
    - webdriver-precore-bidi-runner-base
      - will have to deal with downgrading types of Commands and subscriptions in decoupling from webdriver precore
      - if required just duplicate some of the simple types / newtypes such as JSUInt to get compiling 
    - webdriver-precore-bidi-runner
      - same as for HTTP
  
  - webdriver-precore-driver-control
    - empty library but for depends which should include typed-process, path and pathio

  - webdriver-effectful
    - set up base imports for an effectful library - this will eventually be used to wrap webdriver-precore-extended
  - webdriver-rio-poc
    - as above but using rio
  - webdriver-bluefin-poc
    - as above but using bluefin
  
- test suites for all new libraries 
  - set up named test suites for all libraries importing
    - the corresponding library of the test
    - utils-internal
    - tasty
    - tasty-hunit
    - falsify
  - create a Test.hs module
  
- migrate the demos (not the few unit tests, such as JSON parsing to the webdriver-precore-http-runner and webdriver-precore-bidi-runner test sub-libraries) this is going to require some problem solving regarding how to access shared files that are now all together in teh test suite under test files. what are our options here ? could we use cabal additional files field ? perhaps for now have a separate testResources internal library to return static files as required and share this to test libraries.

once migrated ensure all unused test files are removed from the webdriver-precore test module

- update project and task.json files accordingly 

## overall intructions
- plan first an clarify any issues
- review plan
- make changes starting with runners 
- do compilation checks on the way 
- get all compiling
- do not try running tests
- make sure tests dont get dropped in test migration
- don't change script or ci files yet
- create plan.md and update after every major step such as adding a library and getting compiling
- Do not add any extra functions other than those asked for, this is the first part of a lage transformation 



