# BiDi Connection Failure

## GeckoDriver Start Script

```bash


PORT=4444
WEBSOCKET_PORT=9222

nohup geckodriver \
  --port $PORT \
  --websocket-port $WEBSOCKET_PORT > geckodriver.log 2>&1 &

disown
echo "geckodriver started with PID $! at http://127.0.0.1:$PORT"

# Wait to ensure geckodriver is up and running and detached (neeeded when running from a task)
sleep 5


# Port checking alternatives
echo "Checking ports..."
check_port() {
  (timeout 1 bash -c '</dev/tcp/127.0.0.1/'$1) 2>/dev/null && \
    echo "Port $1 is open" || \
    echo "Port $1 failed"
}

check_port $PORT
check_port $WEBSOCKET_PORT
```

## Connection Code

```Haskell
    catch
      webSocketGo
      ( \(e :: SomeException) -> do
          log $ "WebSocket failure: " <> pack (displayException e)
          -- give time to flush log channel
          threadDelay 100_000
          throwIO e
      )
  where
    webSocketGo = runClient (unpack host) port (unpack path) $ \conn -> do
      log "WebSocket connection established!"
    ...
```
## 1. Starting an Http Session and Connecting on 9222 (Websocket port)

### 1.1 Start Http Connection - Connect BiDi - 9222 / session

```Haskell
biDiDemo :: IO ()
biDiDemo = do
  _path <- sessionViaHttp
  newBidiSessionDemo
    MkBiDiPath
      { host = "127.0.0.1",
        port = 9222,
        path = "/session"
      }
```

```bash
[LOG] MESSAGE RECEIVED: {
    "error": "session not created",
    "id": 1,
    "message": "Maximum number of active sessions",
    ...
```


### 1.2  Start Http Connection - Connect BiDi - 9222 /

```Haskell
biDiDemo :: IO ()
biDiDemo = do
  _path <- sessionViaHttp
  newBidiSessionDemo
    MkBiDiPath
      { host = "127.0.0.1",
        port = 9222,
        path = "/"
      }
```

```bash
[LOG] MESSAGE RECEIVED: {
    "error": "session not created",
    "id": 1,
    "message": "Maximum number of active sessions",
    ...
```


### 1.3 Start Http Connection - Connect BiDi - 9222 ""

looks like we get html 

```Haskell
biDiDemo :: IO ()
biDiDemo = do
  _path <- sessionViaHttp
  newBidiSessionDemo
    MkBiDiPath
      { host = "127.0.0.1",
        port = 9222,
        path = ""
      }
```

```bash
[LOG] WebSocket failure: MalformedResponse (ResponseHead {responseCode = 200, responseMessage = "OK", responseHeaders = [("content-type","text/html;charset=utf-8"),("connection","close"),("server","httpd.js"),("date","Sat, 19 Jul 2025 00:47:12 GMT"),("content-length","361")]}) "Wrong response status or message."
    ...
```

## 2. Create Another Connection Using Websocket Port from Http Connection

```Haskell
biDiDemo :: IO ()
biDiDemo = do
  path <- sessionViaHttp
  newBidiSessionDemo path
```


```bash
...
[LOG] Creating new session...
[LOG] Connecting to WebDriver at MkBiDiPath
  { host = "127.0.0.1"
  , port = 9222
  , path = "/session/393792e3-973e-4c17-8dd9-56b890bc7ab1"
  }
  ...
[LOG] MESSAGE RECEIVED: {
    "error": "session not created",
    "id": 1,
    "message": "Maximum number of active sessions",
    ...
```


## 3. Connect Direct to Websocket Port - No Http Connection

### 3.1 Connect Direct to Websocket Port - No Http Connection ~ "/session"

```Haskell
biDiDemo :: IO ()
biDiDemo =
   newBidiSessionDemo
    MkBiDiPath
      { host = "127.0.0.1",
        port = 9222,
        path = "/session"
      }
```

```bash
[LOG] WebSocket failure: Network.Socket.connect: <socket: 18>: does not exist (Connection refused)
HasCallStack backtrace:
  collectBacktraces, called at libraries/ghc-internal/src/GHC/Internal/Exception.hs:169:13 in ghc-internal:GHC.Internal.Exception
  toExceptionWithBacktrace, called at libraries/ghc-internal/src/GHC/Internal/IO.hs:260:11 in ghc-internal:GHC.Internal.IO
  throwIO, called at libraries/ghc-internal/src/GHC/Internal/Control/Exception/Base.hs:195:43 in ghc-internal:GHC.Internal.Control.Exception.Base

```

### 3.2 Connect Direct to Websocket Port - No Http Connection ~ "/"

- same result as above

### 3.3 Connect Direct to Websocket Port - No Http Connection ~ ""


- same result as above
