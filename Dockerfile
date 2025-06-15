FROM ubuntu:20.04
WORKDIR /app

RUN apt-get update \
    && apt-get install -y \
        build-essential \
        curl \
        libffi-dev \
        libffi7 \
        libgmp-dev \
        libgmp10 \
        libncurses-dev \
        libncurses5 \
        libtinfo5 \
        zlib1g-dev

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \ 
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=9.10.1 \
    BOOTSTRAP_HASKELL_CABAL_VERSION=3.12.1.0 \
    sh

ENV PATH="/root/.ghcup/bin/:$PATH"
ENV DEBIAN_FRONTEND=noninteractive

RUN apt install -y firefox \
    && curl -L https://github.com/mozilla/geckodriver/releases/download/v0.36.0/geckodriver-v0.36.0-linux64.tar.gz | tar xz -C /usr/local/bin

COPY webdriver-precore/webdriver-precore.cabal webdriver-precore/webdriver-precore.cabal
COPY webdriver-examples/webdriver-examples.cabal webdriver-examples/webdriver-examples.cabal
COPY cabal.project .
RUN cabal v2-build all --only-dependencies --enable-tests
RUN cabal install tasty-discover

COPY . .
RUN cabal v2-build all --enable-tests
