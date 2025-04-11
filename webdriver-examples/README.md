# webdriver-examples

not complete or best practice 

basic logging
sleeps
https://the-internet.herokuapp.com/
no work has gone into making these tests robust 
minimal API

hits all the end points

best way to browse functionality VSCode using the eval 


[demo tests are here](./driver-demo-e2e/WebDriverE2EDemoTest.hs)
[runner](./driver-demo-e2e/IORunner.hs)
[API](./driver-demo-e2e/IOAPI.hs)

geckoDriver on linux has been tested with ChromeADriver as well

Install gheckodriver

```
~/repos/webdriver/webdriver-examples$ pkill -f geckodriver || true  && geckodriver --log trace
1744331627146   geckodriver     INFO    Listening on 127.0.0.1:4444
```


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

## Generating Tests
   - need to install tasty discover: ``cabal install tasty-discover``
  
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
capabilites as follows see (capsWithCustomFirefoxProfile):

```firefoxArgs = Just ["-profile", "./webdriver-examples/driver-demo-e2e/.profile/FirefoxWebDriverProfile"]```

then it works.
-}

### Problems with setting 64 bit encrypted profiles

Setting the profile with a 64 bit encoded string did not work for me. The driver could not access the folder it unpacked in the `\tmp` directory. This may be a permissions issue on my machine so others' mileage may differ. See ``capsWithCustomFirefoxProfileNotWorking``
  


## TODO

- [x] fix capabilities
- [x] rename from Pyrethrum
- [x] finish WebDriverError
- [x] check parser of timeouts all other parseJSON has been changed to by name
- [x] create monorepo structure
- [ ] update readme
  - [ ] include notes on running tests and why the tests are there / what they cover
  - [ ] include notes on profile issues (firefox)
- [x] Haddock
  - [ ] synopsis 
  - [ ] description
  - [ ] Spec et. al.
- [x] get tests working locally again
- [ ] CI
  - [ ] get tests and E2E working in CI
- [ ] update replace
  - [ ] docker file
  - [ ] dev-container
  - [ ] scripts
- [ ] fork the internet into Pyrethrum org
- [x] update tasks
- [ ] sort out / delete misbehaving tasks (permissions)
  - [ ] run geckodriver - doesn't work 
  - [ ] run haddock - doesn't open in browser
- [ ] work out whats up with the trailing fields warning on tested with
  - [ ] meaning of tested with
  - [ ] why warning
- [ ] add checks for e2e when profile dir is not there
- [ ] review compiler switches
- [ ] review licence
- [ ] add all to github container repo (check vs docker hub)
- [ ] change log
- [ ] hackage
- [ ] stackage
