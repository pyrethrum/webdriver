# webdriver-examples

## About These Examples

These examples demonstrate a minimal wrapper for the `webdriver-precore` library, to the extent that it can be used to drive a browser. Utility functions and other "production" concerns have been deliberately omitted.

Differences between these examples and how `webdriver-precore`  would be used in developing a full framework include:
 * no utility functions or automatic browser and session management
 * hard coded config options
 * hard coded console logging
 * sleeps and arbitrary browser info being logged throughout the tests (so the user can observe whats happening)
 * little effort has been put into ensuring these examples are robust (such as sophisticated waits)

These examples hit every [W3C endpoint]() exposed by [webdriver-precore]() while interaction with [the internet](https://the-internet.herokuapp.com)

## Core Modules

The core modules are as follows:

### The [Runner](./driver-demo-e2e/IORunner.hs)

Takes the 

### The [API](./driver-demo-e2e/IOAPI.hs)

Takes the 

### The Actual Examples [Demo Tests Are here](./driver-demo-e2e/WebDriverE2EDemoTest.hs)

A more detailed explanation of how the modules fit together can be found  in the [webdriver-precore readme]().


## Running Examples

### Prerequisites

## Executing the Tests

best way to browse functionality VSCode using the eval 

 need to install tasty discover: ``cabal install tasty-discover``

geckoDriver on linux has been tested with ChromeADriver as well

Install gheckodriver

```
~/repos/webdriver/webdriver-examples$ pkill -f geckodriver || true  && geckodriver --log trace
1744331627146   geckodriver     INFO    Listening on 127.0.0.1:4444
```

Install gheckodriver

### Update 'Configuration Options'


### Gheckodriver and Firfox Profile Issues
~/repos/webdriver$ pkill -f chromedriver || true && chromedriver --log-level=ALL --port=4444
Starting ChromeDriver 135.0.7049.52 (9ba7e609d28c509a8ce9265c2247065d8d251173-refs/branch-heads/7049_41@{#4}) on port 4444
Only local connections are allowed.
Please see https://chromedriver.chromium.org/security-considerations for suggestions on keeping ChromeDriver safe.
ChromeDriver was started successfully on port 4444.

https://github.com/mozilla/geckodriver/releases

about:profiles

pkill -f chromedriver || true && chromedriver --log-level=ALL --port=4444

The webdriver WC3 API represented as a Haskell type

# Work In Progress

## Git LFS
  - need to install lfs locally
   ```
    curl -s https://packagecloud.io/install/repositories/github/git-lfs/script.deb.sh | sudo bash
    sudo apt-get install git-lfs
    git lfs install
  ```

  
## Running E2E Tests
- need to install
  - firefox
  - geckodriver
 
  - need to run:

    ``pkill -f geckodriver || true  && geckodriver &``

    or with logging

    ``pkill -f geckodriver || true  && geckodriver --log trace``

### Firefox Profile Issues

If you get an error when running tests like: **Your Firefox profile cannot be loaded. It may be missing or inaccessible.** you will need to run using a custom profile.

1. unzip `./webdriver-examples/driver-demo-e2e/FirefoxWebDriverProfile.zip => to "./webdriver-examples/driver-demo-e2e/.profile/FirefoxWebDriverProfile"`
2. ensure `useCustomProfile = True`

  ```haskell
    module WebDriverDemoStubsTest
    --- 
    mkExtendedTimeoutsSession :: IO SessionId
    mkExtendedTimeoutsSession = do
    let useCustomProfile = True
    ---
  ```

{-
This fails on my machine with the following error:

```Your Firefox profile cannot be loaded. It may be missing or inaccessible.```

This appears to be due to the profile being unpacked into tmp and the driver not being able to access it.
If I copy the unpacked profile to "./webdriver-examples/driver-demo-e2e/.profile/FirefoxWebDriverProfile" and reference in
capabilities as follows see (capsWithCustomFirefoxProfile):

```firefoxArgs = Just ["-profile", "./webdriver-examples/driver-demo-e2e/.profile/FirefoxWebDriverProfile"]```

then it works.
-}

### Problems with setting 64 bit encrypted profiles

Setting the profile with a 64 bit encoded string did not work for me. The driver could not access the folder it unpacked in the `\tmp` directory. This may be a permissions issue on my machine so others' mileage may differ. See ``capsWithCustomFirefoxProfileNotWorking``
  
