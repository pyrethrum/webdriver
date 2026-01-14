# Container Files

This document describes the Docker container hierarchy and their associated build/push scripts.

## Dependency Diagram

```
ubuntu:25.04
    └── dockerfile-haskell (ghcr.io/pyrethrum/haskell)
            └── dockerfile-webdriver-precore (ghcr.io/pyrethrum/webdriver-precore)
                    └── dockerfile-webdriver-precore-firefox (ghcr.io/pyrethrum/webdriver-precore-firefox)
                            ├── dockerfile-ci-test (ghcr.io/pyrethrum/ci-test)
                            └── devcontainer.json (dev container)
```

## Container Files Table

| File Name | Tag Name | Depends On | Build Script | Push Script | Description |
|-----------|----------|------------|--------------|-------------|-------------|
| [dockerfile-haskell](dockerfile-haskell) | ghcr.io/pyrethrum/haskell:latest | [ubuntu:25.04](https://hub.docker.com/_/ubuntu) | [docker-build-haskell.sh](docker-build-haskell.sh) | [docker-push-haskell.sh](docker-push-haskell.sh) | Base Haskell development image with GHCup, GHC, and essential build tools |
| [dockerfile-webdriver-precore](dockerfile-webdriver-precore) | ghcr.io/pyrethrum/webdriver-precore:latest | [dockerfile-haskell](dockerfile-haskell) | [docker-build-webdriver-precore.sh](docker-build-webdriver-precore.sh) | [docker-push-webdriver-precore.sh](docker-push-webdriver-precore.sh) | Clones webdriver repo and pre-builds dependencies with cabal |
| [dockerfile-webdriver-precore-firefox](dockerfile-webdriver-precore-firefox) | ghcr.io/pyrethrum/webdriver-precore-firefox:latest | [dockerfile-webdriver-precore](dockerfile-webdriver-precore) | [docker-build-webdriver-precore-firefox.sh](docker-build-webdriver-precore-firefox.sh) | [docker-push-webdriver-precore-firefox.sh](docker-push-webdriver-precore-firefox.sh) | Adds Firefox and geckodriver for browser automation testing |
| [dockerfile-ci-test](dockerfile-ci-test) | ghcr.io/pyrethrum/ci-test:latest | [dockerfile-webdriver-precore-firefox](dockerfile-webdriver-precore-firefox) | [docker-build-ci-test.sh](docker-build-ci-test.sh) | [docker-push-ci-test.sh](docker-push-ci-test.sh) | CI container that copies local source files and configures test environment |
| [devcontainer.json](../.devcontainer/devcontainer.json) | None | [dockerfile-webdriver-precore-firefox](dockerfile-webdriver-precore-firefox) | None | None | VS Code dev container with desktop-lite feature for noVNC browser viewing |
