# webdriver

![build](https://github.com/pyrethrum/webdriver/actions/workflows/cicd.yaml/badge.svg?branch=main)
[![Hackage](https://img.shields.io/hackage/v/webdriver-precore.svg)](https://hackage.haskell.org/package/webdriver-precore)
[![Stackage Nightly](http://stackage.org/package/webdriver-precore/badge/nightly)](http://stackage.org/nightly/package/webdriver-precore)


## Subprojects

This repository contains a single sub-repo:

### 1. [webdriver-precore](./webdriver-precore/README.md)

 Typed definitions for the W3C WebDriver Protocol, supporting both the [HTTP] and the [BiDi] protocols.

This is a library intended to be used as a base for other libraries that provide a WebDriver client implementation and higher level functions.

More info can be found in the [webdriver-precore README](./webdriver-precore/README.md) and the [Hackage Docs](https://hackage.haskell.org/package/webdriver-precore)

For runnable demos and source code for an example client implementation see the [webdriver-precore test directory](./webdriver-precore/test#readme).

## Future Plans

Over the coming months we intend to build out a number of sub-libraries and tools, culminating in a fully featured webdriver client.

<!-- Link References -->

[BiDi]: https://www.w3.org/TR/2026/WD-webdriver-bidi-20260109/
[HTTP]: https://www.w3.org/TR/2025/WD-webdriver2-20251028/