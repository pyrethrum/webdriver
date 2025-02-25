# webdriver-precore

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

1. unzip `./test-e2e/FirefoxWebDriverProfile.zip => to "./test-e2e/.profile/FirefoxWebDriverProfile"`
2. ensure `useCustomProfile = True`

  ```haskell
    module WebDriverDemoStubsTest
    --- 
    mkExtendedTimeoutsSession :: IO SessionId
    mkExtendedTimeoutsSession = do
    let useCustomProfile = True
    ---
  ```

### Problems with setting 64 bit encrypted profiles

Setting the profile with a 64 bit encoded string did not work for me. The driver could not access the folder it unpacked in the `\tmp` directory. This may be a permissions issue on my machine so others' mileage may differ. See ``capsWithCustomFirefoxProfileNotWorking``
  


## TODO

- [x] fix capabilities
- [x] rename from Pyrethrum
- [x] finish WebDriverError
- [x] check parser of timeouts all other parseJSON has been changed to by name
- [ ] update readme
  - [ ] include notes on running tests and why the tests are there / what they cover
  - [ ] include notes on profile issues (firefox)
- [ ] Haddock
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
- [ ] review compiler switches
- [ ] add all to github container repo (check vs docker hub)
- [ ] change log
- [ ] hackage
- [ ] stackage
