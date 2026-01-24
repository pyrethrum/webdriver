# TODO


## deferred
 - [ ] edge driver
 - [ ] chrome - CI
 - [ ] test with driver not running
 - [ ] rearrange docker - move hls install to just before dev-container
 - [ ] ci see if can compile test server once

## final restructure

- [ ] [update readme in similar style to](https://github.com/nikita-volkov/hasql)
- [ ] restore legacy test / code for HTTP

### interim
- use shared libraries

### use flag to turn off integration testing in hackage / stackage
```
flag build-test-utils
  description: Build with test utilities (for development)
  default: False
  manual: True

test-suite test
  if flag(build-test-utils)
    build-depends: webdriver-test-utils
  else
    -- Use simplified tests or skip certain tests

```

### immediate post restructure

 [ ] all tests working
 [ ] read unliftIO
 [ ] read RIO
 [ ] reexport base test
 [ ] all patterns

