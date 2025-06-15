let Config = ./ConfigType.dhall

let config : Config = {
  useFirefox = True,
  firefoxHeadless = True,
  customFirefoxProfilePath = None Text,
  wantConsoleLogging = False
}

in
  config