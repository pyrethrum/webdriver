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
      , wantConsoleLogging : Bool
      }

-- Config value
let browser : Browser = 
      Browser.Firefox
        { headless = False
        , profilePath = None Text
        }

let config : Config = 
      { browser = browser
      , wantConsoleLogging = False
      }

in config
