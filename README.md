# webdriver-precore

The webdriver WC3 API represented as a Haskell type

## Note
- need to install
  - geckodriver
  - tasty discover
  - need to run:
    ``pkill -f geckodriver || true  && geckodriver &``
    before running E2E tests
  - note 
    - having issues with geko driver with hyphen in path
    - padding

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
