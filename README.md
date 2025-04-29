# webdriver

![build](https://github.com/pyrethrum/webdriver/actions/workflows/cicd.yaml/badge.svg?branch=main)
[![Hackage](https://img.shields.io/hackage/v/webdriver-precore.svg)](https://hackage.haskell.org/package/webdriver-precore)
[![Stackage Nightly](http://stackage.org/package/webdriver-precore/badge/nightly)](http://stackage.org/nightly/package/webdriver-precore)


This repository contains two sub-repos:

## 1. [webdriver-precore](./webdriver-precore/README.md)

Typed definitions for the endpoints of the [W3C Webdriver](https://www.w3.org/TR/2025/WD-webdriver2-20250306).

This is a library intended to be used as a base for other libraries that provide a WebDriver client implementation and higher level functions.

More info can be found in the [README](./webdriver-precore/README.md) and the [Hackage Docs](https://hackage.haskell.org/package/webdriver-precore)

## 2. [webdriver-examples](./webdriver-examples/README.md)

Examples that demonstrate a minimal wrapper implementation around the [webdriver-precore](./webdriver-precore/README.md) library for basic browser automation.

More info can be found in the [README](./webdriver-examples/README.md)

