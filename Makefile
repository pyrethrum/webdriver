.PHONY: build test
.SILENT: build test

IMAGE ?= webdriver

build:
	docker build -t $(IMAGE) .

test: build
	docker run $(IMAGE) bash -c \
	  'geckodriver --binary /usr/bin/firefox & cabal test all'
