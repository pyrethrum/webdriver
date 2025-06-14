.PHONY: build test
.SILENT: build test

IMAGE ?= webdriver

build:
	docker build -t $(IMAGE) .

test:
	docker run $(IMAGE) bash -c \
		'MOZ_REMOTE_SETTINGS_DEVTOOLS=1 \
		 MOZ_HEADLESS=1 \
		 geckodriver --binary /usr/bin/firefox & \cabal test all'
