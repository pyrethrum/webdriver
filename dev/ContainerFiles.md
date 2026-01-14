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

| File Name | Tag Name | Depends On | Description |
|-----------|----------|------------|-------------|
| [dockerfile-haskell](dockerfile-haskell) | ghcr.io/pyrethrum/haskell:latest | [ubuntu:25.04](https://hub.docker.com/_/ubuntu) | Base Haskell development image with GHCup, GHC, and essential build tools |
| [dockerfile-webdriver-precore](dockerfile-webdriver-precore) | ghcr.io/pyrethrum/webdriver-precore:latest | [dockerfile-haskell](dockerfile-haskell) | Clones webdriver repo and pre-builds dependencies with cabal |
| [dockerfile-webdriver-precore-firefox](dockerfile-webdriver-precore-firefox) | ghcr.io/pyrethrum/webdriver-precore-firefox:latest | [dockerfile-webdriver-precore](dockerfile-webdriver-precore) | Adds Firefox and geckodriver for browser automation testing |
| [dockerfile-ci-test](dockerfile-ci-test) | ghcr.io/pyrethrum/ci-test:latest | [dockerfile-webdriver-precore-firefox](dockerfile-webdriver-precore-firefox) | CI container that copies local source files and configures test environment |
| [devcontainer.json](../.devcontainer/devcontainer.json) | None | [dockerfile-webdriver-precore-firefox](dockerfile-webdriver-precore-firefox) | VS Code dev container |

## Build & Push Scripts

<details>
<summary><strong>dockerfile-haskell</strong></summary>

Build:
```bash
bash docker-build-haskell.sh
```

Push:
```bash
bash docker-push-haskell.sh
```

</details>

<details>
<summary><strong>dockerfile-webdriver-precore</strong></summary>

Build:
```bash
bash docker-build-webdriver-precore.sh
```

Push:
```bash
bash docker-push-webdriver-precore.sh
```

</details>

<details>
<summary><strong>dockerfile-webdriver-precore-firefox</strong></summary>

Build:
```bash
bash docker-build-webdriver-precore-firefox.sh
```

Push:
```bash
bash docker-push-webdriver-precore-firefox.sh
```

</details>

<details>
<summary><strong>dockerfile-ci-test</strong></summary>

Build:
```bash
bash docker-build-ci-test.sh
```

Push:
```bash
bash docker-push-ci-test.sh
```

</details>
