FROM ubuntu:20.04
WORKDIR /webdriver


# Install dependencies for Firefox
RUN apt-get update \
    && apt-get install -y --fix-missing \
    wget \
    xz-utils \
    libgtk-3-0 \
    libdbus-glib-1-2 \
    libasound2 \
    libx11-xcb1 \
    libxt6 \
    libpci3 \
    && rm -rf /var/lib/apt/lists/*

# Install latest Firefox from Mozilla (downloads as .tar.xz)
RUN wget -O /tmp/firefox.tar.xz "https://download.mozilla.org/?product=firefox-latest-ssl&os=linux64&lang=en-US" \
    && tar -xJf /tmp/firefox.tar.xz -C /opt \
    && ln -s /opt/firefox/firefox /usr/local/bin/firefox \
    && rm /tmp/firefox.tar.xz


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

# Install geckodriver
RUN curl -L https://github.com/mozilla/geckodriver/releases/download/v0.36.0/geckodriver-v0.36.0-linux64.tar.gz | tar xz -C /usr/local/bin

COPY webdriver-precore/webdriver-precore.cabal webdriver-precore/webdriver-precore.cabal
COPY webdriver-precore/test-server/test-server.cabal webdriver-precore/test-server/test-server.cabal
COPY cabal.project .
RUN cabal v2-build all --only-dependencies --enable-tests

# Copy all files and directories from the current build context (.) 
COPY . .
# RUN echo "==== LISTING WEBDRIVER DIRECTORYY ====" && \
#     find . -not -path "./.git/*" -not -path "*/dist-newstyle/*" -ls && \
#     echo "==== END LISTING ===="
COPY dev/config-ci.dhall webdriver-precore/test/.config/config.dhall
# remove debug local config file if it exists - we want CI builds to use config.dhall
RUN rm -f cabal.project.local
# RUN echo "==== BEGIN CONFIG FILE ====" && \
#     cat webdriver-precore/test/.config/config.dhall && \
#     echo "==== END CONFIG FILE ===="

# RUN cabal clean
RUN cabal v2-build all --enable-tests
RUN geckodriver --version && firefox --version
