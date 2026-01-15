# webdriver-precore

![build](https://github.com/pyrethrum/webdriver/actions/workflows/cicd.yaml/badge.svg?branch=main)
[![Hackage](https://img.shields.io/hackage/v/webdriver-precore.svg)](https://hackage.haskell.org/package/webdriver-precore)
[![Stackage Nightly](http://stackage.org/package/webdriver-precore/badge/nightly)](http://stackage.org/nightly/package/webdriver-precore)


<!-- 
Adapt and move when ready
![CI](https://github.com/commercialhaskell/path/workflows/CI/badge.svg?branch=master)
[![Stackage LTS](http://stackage.org/package/path/badge/lts)](http://stackage.org/lts/package/path)
[![Stackage Nightly](http://stackage.org/package/path/badge/nightly)](http://stackage.org/nightly/package/path) 
-->


## What is This Library?

This library provides typed definitions for the W3C WebDriver Protocol, supporting both the [HTML](https://www.w3.org/TR/2025/WD-webdriver2-20251028/) and the [BiDi](https://www.w3.org/TR/2025/WD-webdriver-bidi-20251212/) protocols.

This library is intended as a foundation for building WebDriver client implementations. **It is type constructors only**, and does not include any executable client code.

If you are writing a webdriver client, this library will save you the effort of analysing the specs and implementing the protocol types and JSON instances.

If you are looking for a library to enable you to interact with web pages directly then you need a fully implemented web client library **which this library is not**.

For a fully implemented webdriver client, consider an alternative such as [haskell-webdriver](https://github.com/haskell-webdriver/haskell-webdriver#readme)

## Why This Library?

Several libraries provide WebDriver bindings for Haskell. However, when development on this library began, the existing options were either unmaintained, dependent on Selenium, or tightly coupled with larger "batteries included" testing frameworks.

We, the authors of this library, are building our own stand-alone test framework. To support browser based testing within this framework we're first creating a series of independent low-level libraries. This library is the first in that series. Our aim is to make each of our low level libraries broadly useful to others, outside its use within our own framework. 

### Core Principles
- **Direct W3C WebDriver Implementation**  
  - No Selenium dependency  
  - Full control over protocol handling  
  *Note: the [W3C WebDriver standard](https://www.w3.org/TR/webdriver2/) is an initiative driven largely by the core Selenium contributors. It provides a uniform HTTP API to drive browsers, and can be leveraged by any library, including Selenium.*

- **Minimalist Design**  
  - Boring Haskell
  - Few external dependencies  

- **Enable a Layered Architecture**  
  - Provide an unopinionated WebDriver client for use in higher level libraries

### Library Non-Goals
  
The following features are not included in this library. They belong in downstream libraries.
  * Convenience or utility functions, that do not directly correspond to an endpoint on the W3C spec.
  * Transformers, effects or similar abstractions. 
  * Bespoke variations from the spec to accommodate non-standard driver behaviour.

### Acknowledgements

This library would not have been possible without the prior work in: 

**Haskell (particularly)**:
* [haskell-webdriver](https://hackage.haskell.org/package/webdriver)
* [webdriver-w3c](https://hackage.haskell.org/package/webdriver-w3c)

**Selenium and WebDriver Standards**:

The decade+ efforts of the [Selenium](https://www.selenium.dev/) maintainers, both in forging the way with Selenium and their ongoing work in the development of the W3C standards, both [HTTP](https://www.w3.org/TR/webdriver2/) and [BiDi](https://www.w3.org/TR/webdriver-bidi/)

## Further Details

For further details on the structure and use of this library see the [Hackage Docs](https://hackage.haskell.org/package/webdriver-precore).

For runnable demos and source code for an example client implementation see the [test directory of this repository](./test/README.md).
