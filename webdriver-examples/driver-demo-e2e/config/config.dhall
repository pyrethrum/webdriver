let ConfigType = 
  { useFirefox : Bool
  , firefoxHeadless : Bool
  , customFirefoxProfilePath : Optional Text
  , wantConsoleLogging : Bool
  }

let configDefault = 
  { useFirefox = False
  , firefoxHeadless = True
  , customFirefoxProfilePath = None Text
  , wantConsoleLogging = False
  }

let Config = 
  { Type = ConfigType
  , default = configDefault
  }

in Config