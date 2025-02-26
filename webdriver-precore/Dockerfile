FROM ubuntu:20.04 AS builder

ARG GHC_VERSION=9.10.1
ARG CABAL_VERSION=3.12.1.0

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
        libtinfo5

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \ 
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=${GHC_VERSION} \
    BOOTSTRAP_HASKELL_CABAL_VERSION=${CABAL_VERSION} \
    sh

COPY . .
