# webdriver-precore

The webdriver WC3 API represented as a Haskell type

## Note
- need to install
  - geckodriver
  - tasty discover
  - need to run:
    ``pkill -f geckodriver || true  && geckodriver &``

    ``pkill -f geckodriver || true  && geckodriver --log debug``

    before running E2E tests
  - note 
    - profiles - missing progile
  
  - need to install lfs locally
  - ```
    curl -s https://packagecloud.io/install/repositories/github/git-lfs/script.deb.sh | sudo bash
    sudo apt-get install git-lfs
    git lfs install
  ```

## TODO

- [ ] fix capabilities
  - [ ] https://developer.mozilla.org/en-US/docs/Web/WebDriver/Capabilities
  - [ ] https://mucsi96.gitbook.io/w3c-webdriver/capabilities
- [ ] rename from Pyrethrum
- [ ] finish WebDriverError
- [ ] check parser of timeouts has been changed to by name
- [ ] update readme
  - [ ] include notes on profile isssues (firefox)
- [ ] Haddock
- [ ] get tests working locally again
- [ ] CI
  - [ ] get tests and E2E working in CI
- [ ] update replace
  - [ ] docker file
  - [ ] dev-container
  - [ ] scripts
- [ ] update tasks
- [ ] add all to github container repo (check vs docker hub)
- [ ] hackage
- [ ] stackage
