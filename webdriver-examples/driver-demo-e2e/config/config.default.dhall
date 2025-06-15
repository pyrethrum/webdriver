let ConfigType = 
  { useFirefox : Bool
  , firefoxHeadless : Bool
  , customFirefoxProfilePath : Optional Text
  , wantConsoleLogging : Bool
  }

let config = 
  { useFirefox = False
  , firefoxHeadless = True
  , customFirefoxProfilePath = None Text
  , wantConsoleLogging = False
  }

let Config = 
  { Type = ConfigType
  , config = config
  }

in Config