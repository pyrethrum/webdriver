.PHONY: build test
.SILENT: build test

IMAGE ?= webdriver

build:
	docker build -t $(IMAGE) .

test: build
	docker run $(IMAGE) bash -c \
      'bash ./dev/start-geckodriver.sh & cabal test all'
#       'geckodriver --version && firefox --version && bash ./dev/start-geckodriver.sh & cabal test all'
