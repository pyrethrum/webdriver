{ browser =
    < Chrome
    | Firefox : { headless : Bool, profilePath : Optional Text }
    >.Firefox
      { headless = False, profilePath = None Text }
, wantConsoleLogging = False
}