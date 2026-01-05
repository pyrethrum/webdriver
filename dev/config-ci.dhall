-- Config types
let Browser = 
      < Chrome
      | Firefox : 
          { headless : Bool
          , profilePath : Optional Text 
          }
      >

let Config = 
      { browser : Browser
      , logging : Bool
      , httpUrl : Text
      , httpPort : Natural
      , pauseMS : Natural
      }

-- Config value
let browser : Browser = 
      Browser.Firefox 
        { headless = True
        , profilePath = None Text
        }

let config : Config = 
      { browser = browser
      , logging = True
      , httpUrl = "127.0.0.1"
      , httpPort = 4444
      , pauseMS = 0
      }

in config
