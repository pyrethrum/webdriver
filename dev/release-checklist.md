## webdriver-precore

Releases are somewhat manual at this stage. 

1. [ ] check the latest version of [webdriver](https://www.w3.org/standards/history/webdriver2/) is covered
   - [ ] update all links in documentation and source by doing a global replace for the date portion of the url
   - [ ] diff the [spec](https://github.com/w3c/webdriver/blob/master/index.html) from the last version and ensure any api changes are implemented
   - [ ] manually copy the latest endpoints to `endPointsCopiedFromSpc` and errors to `errorsFromSpec`
3. [ ] build all with cabal.project files pointing to the latest Stackage nightly
4. [ ] run all unit tests in `webdriver-precore`
5. [ ] run all unit tests in `webdriver-examples` - note these may be a bit flaky depending on how "the internet" is running. Failures in these demo tests don't necessarily mean the precore API is failing, as long as the integration between the driver and the API is OK we should be good to go.
6. [ ] check the READMEs in both `webdriver-precore` and `webdriver-examples` are up to date, particularly the examples
7. [ ] rebuild and push the `haskell` and `webdriver-precore` docker images (see the `dev` folder)
   - test the dev-container still works in line with the instructions in the `webdriver-examples` README
8. [ ] check what version number changes are required as per [PVP](https://pvp.haskell.org/#decision-tree)