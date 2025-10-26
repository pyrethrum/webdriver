module BiDi.Demos.ScriptDemos where

-- custom import needed to disambiguate capabilities

import BiDi.BiDiActions (BiDiActions (..))
import BiDi.DemoUtils
import Data.Aeson (ToJSON (..))
import Data.Maybe (catMaybes)
import Data.Text (isInfixOf, pack)
import IOUtils (DemoUtils (..))
import WebDriverPreCore.BiDi.CoreTypes (JSUInt (..))
import WebDriverPreCore.BiDi.Protocol
import qualified WebDriverPreCore.BiDi.Script as Script
import WebDriverPreCore.Internal.AesonUtils (jsonToText)
import Prelude hiding (log, putStrLn)
import WebDriverPreCore.BiDi.CoreTypes (StringValue(..))
import Const (milliseconds)

{-

##### Script #####
1. scriptAddPreloadScript :: DONE
2. scriptCallFunction :: DONE
3. scriptDisown :: DONE
4. scriptEvaluate :: DONE
5. scriptGetRealms :: DONE
6. scriptRemovePreloadScript :: DONE

-- TODO when using script - get browsingContextGetTree with originalOpener
  -- list not supported yet in geckodriver

-}

-- >>> runDemo scriptEvaluateAllPrimitiveTypesDemo
scriptEvaluateAllPrimitiveTypesDemo :: BiDiDemo
scriptEvaluateAllPrimitiveTypesDemo =
  demo "Script - Evaluate All PrimitiveProtocolValue Types" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi
      let baseEval =
            MkEvaluate
              { expression = "alert('Hello from Pyrethrum BiDi!')",
                target =
                  ContextTarget $
                    MkContextTarget
                      { context = bc,
                        sandbox = Nothing
                      },
                awaitPromise = True,
                resultOwnership = Nothing,
                serializationOptions = Nothing
              }

      logTxt "Test 1: Undefined evaluation - returns UndefinedValue"
      r1 <- scriptEvaluate $ baseEval {expression = "undefined"}
      logShow "Script evaluation result - undefined" r1
      pause

      logTxt "Test 2: Null evaluation - returns NullValue"
      r2 <- scriptEvaluate $ baseEval {expression = "null"}
      logShow "Script evaluation result - null" r2
      pause

      logTxt "Test 3: String evaluation - returns StringValue"
      r3 <- scriptEvaluate $ baseEval {expression = "'Hello from BiDi Script!'"}
      logShow "Script evaluation result - string" r3
      pause

      logTxt "Test 4: String evaluation with escape characters"
      r4 <- scriptEvaluate $ baseEval {expression = "'Line 1\\nLine 2\\tTabbed'"}
      logShow "Script evaluation result - string with escapes" r4
      pause

      logTxt "Test 5: Number evaluation - integer"
      r5 <- scriptEvaluate $ baseEval {expression = "42"}
      logShow "Script evaluation result - number (integer)" r5
      pause

      logTxt "Test 6: Number evaluation - float"
      r6 <- scriptEvaluate $ baseEval {expression = "3.14159"}
      logShow "Script evaluation result - number (float)" r6
      pause

      logTxt "Test 7: Number evaluation - negative"
      r7 <- scriptEvaluate $ baseEval {expression = "-123.456"}
      logShow "Script evaluation result - number (negative)" r7
      pause

      logTxt "Test 8: Number evaluation - zero"
      r8 <- scriptEvaluate $ baseEval {expression = "0"}
      logShow "Script evaluation result - number (zero)" r8
      pause

      logTxt "Test 9: Special Number - NaN"
      r9 <- scriptEvaluate $ baseEval {expression = "NaN"}
      logShow "Script evaluation result - NaN" r9
      pause

      logTxt "Test 10: Special Number - Negative Zero"
      r10 <- scriptEvaluate $ baseEval {expression = "-0"}
      logShow "Script evaluation result - negative zero" r10
      pause

      logTxt "Test 11: Special Number - Infinity"
      r11 <- scriptEvaluate $ baseEval {expression = "Infinity"}
      logShow "Script evaluation result - Infinity" r11
      pause

      logTxt "Test 12: Special Number - Negative Infinity"
      r12 <- scriptEvaluate $ baseEval {expression = "-Infinity"}
      logShow "Script evaluation result - Negative Infinity" r12
      pause

      logTxt "Test 13: Special Number - Division by zero (Infinity)"
      r13 <- scriptEvaluate $ baseEval {expression = "1 / 0"}
      logShow "Script evaluation result - 1/0 = Infinity" r13
      pause

      logTxt "Test 14: Special Number - Invalid operation (NaN)"
      r14 <- scriptEvaluate $ baseEval {expression = "Math.sqrt(-1)"}
      logShow "Script evaluation result - sqrt(-1) = NaN" r14
      pause

      logTxt "Test 15: Boolean evaluation - true"
      r15 <- scriptEvaluate $ baseEval {expression = "true"}
      logShow "Script evaluation result - boolean true" r15
      pause

      logTxt "Test 16: Boolean evaluation - false"
      r16 <- scriptEvaluate $ baseEval {expression = "false"}
      logShow "Script evaluation result - boolean false" r16
      pause

      logTxt "Test 17: Boolean evaluation - truthy expression"
      r17 <- scriptEvaluate $ baseEval {expression = "!!'hello'"}
      logShow "Script evaluation result - !!string = true" r17
      pause

      logTxt "Test 18: Boolean evaluation - falsy expression"
      r18 <- scriptEvaluate $ baseEval {expression = "!!0"}
      logShow "Script evaluation result - !!0 = false" r18
      pause

      logTxt "Test 19: BigInt evaluation - small BigInt"
      r19 <- scriptEvaluate $ baseEval {expression = "42n"}
      logShow "Script evaluation result - BigInt 42n" r19
      pause

      logTxt "Test 20: BigInt evaluation - large BigInt"
      r20 <- scriptEvaluate $ baseEval {expression = "9007199254740991n"}
      logShow "Script evaluation result - BigInt (Number.MAX_SAFE_INTEGER)" r20
      pause

      logTxt "Test 21: BigInt evaluation - very large BigInt"
      r21 <- scriptEvaluate $ baseEval {expression = "12345678901234567890123456789012345678901234567890n"}
      logShow "Script evaluation result - very large BigInt" r21
      pause

      logTxt "Test 22: BigInt evaluation - negative BigInt"
      r22 <- scriptEvaluate $ baseEval {expression = "-9007199254740991n"}
      logShow "Script evaluation result - negative BigInt" r22
      pause

      logTxt "Test 23: BigInt evaluation - zero BigInt"
      r23 <- scriptEvaluate $ baseEval {expression = "0n"}
      logShow "Script evaluation result - BigInt zero" r23
      pause

      logTxt "Test 24: BigInt evaluation - BigInt arithmetic"
      r24 <- scriptEvaluate $ baseEval {expression = "BigInt(123) * BigInt(456)"}
      logShow "Script evaluation result - BigInt arithmetic" r24
      pause

      logTxt "Test 25: Complex expression evaluation - mixed types in comparison"
      r25 <- scriptEvaluate $ baseEval {expression = "typeof 'string' === 'string'"}
      logShow "Script evaluation result - typeof comparison" r25
      pause

      logTxt "Test 26: Complex expression evaluation - Number.isNaN"
      r26 <- scriptEvaluate $ baseEval {expression = "Number.isNaN(NaN)"}
      logShow "Script evaluation result - Number.isNaN(NaN)" r26
      pause

      logTxt "Test 27: Complex expression evaluation - Number.isFinite"
      r27 <- scriptEvaluate $ baseEval {expression = "Number.isFinite(42)"}
      logShow "Script evaluation result - Number.isFinite(42)" r27
      pause

      logTxt "Test 28: Empty string evaluation"
      r28 <- scriptEvaluate $ baseEval {expression = "''"}
      logShow "Script evaluation result - empty string" r28
      pause

      logTxt "Test 29: Unicode string evaluation"
      r29 <- scriptEvaluate $ baseEval {expression = "'Hello 🌍 World! αβγ 中文'"}
      logShow "Script evaluation result - unicode string" r29
      pause

      logTxt "Test 30: Mathematical constants"
      r30 <- scriptEvaluate $ baseEval {expression = "Math.PI"}
      logShow "Script evaluation result - Math.PI" r30
      pause

-- >>> runDemo scriptEvaluateAdvancedDemo
scriptEvaluateAdvancedDemo :: BiDiDemo
scriptEvaluateAdvancedDemo =
  demo "Script - Evaluate Advanced Types and Edge Cases" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi
      let baseEval =
            MkEvaluate
              { expression = "",
                target =
                  ContextTarget $
                    MkContextTarget
                      { context = bc,
                        sandbox = Nothing
                      },
                awaitPromise = False,
                resultOwnership = Nothing,
                serializationOptions = Nothing
              }

      logTxt "Advanced Test 1: Array evaluation (non-primitive)"
      a1 <- scriptEvaluate $ baseEval {expression = "[1, 2, 3, 'hello', true, null]"}
      logShow "Script evaluation result - array" a1
      pause

      logTxt "Advanced Test 2: Object evaluation (non-primitive)"
      a2 <- scriptEvaluate $ baseEval {expression = "{ name: 'BiDi', version: 1.0, active: true }"}
      logShow "Script evaluation result - object" a2
      pause

      logTxt "Advanced Test 3: Function evaluation (non-primitive)"
      a3 <- scriptEvaluate $ baseEval {expression = "function greet(name) { return 'Hello, ' + name; }"}
      logShow "Script evaluation result - function" a3
      pause

      logTxt "Advanced Test 4: Date evaluation (non-primitive)"
      a4 <- scriptEvaluate $ baseEval {expression = "new Date('2024-01-15T10:30:00Z')"}
      logShow "Script evaluation result - date" a4
      pause

      logTxt "Advanced Test 5: RegExp evaluation (non-primitive)"
      a5 <- scriptEvaluate $ baseEval {expression = "/^[a-z]+$/gi"}
      logShow "Script evaluation result - regexp" a5
      pause

      logTxt "Advanced Test 6: Promise evaluation - resolved (awaitPromise=False)"
      a6 <- scriptEvaluate $ baseEval {expression = "Promise.resolve('resolved value')"}
      logShow "Script evaluation result - promise (not awaited)" a6
      pause

      logTxt "Advanced Test 7: Promise evaluation - resolved (awaitPromise=True)"
      a7 <- scriptEvaluate $ baseEval {expression = "Promise.resolve('resolved value')", awaitPromise = True}
      logShow "Script evaluation result - promise (awaited)" a7
      pause

      logTxt "Advanced Test 8: Symbol evaluation (non-primitive)"
      a8 <- scriptEvaluate $ baseEval {expression = "Symbol('test-symbol')"}
      logShow "Script evaluation result - symbol" a8
      pause

      logTxt "Advanced Test 9: Error evaluation"
      a9 <- scriptEvaluate $ baseEval {expression = "new Error('Test error message')"}
      logShow "Script evaluation result - error object" a9
      pause

      logTxt "Advanced Test 10: Throw error evaluation (should produce exception result)"
      a10 <- scriptEvaluate $ baseEval {expression = "throw new Error('Intentional test error')"}
      logShow "Script evaluation result - thrown error" a10
      pause

      logTxt "Advanced Test 11: DOM element evaluation (if available)"
      a11 <- scriptEvaluate $ baseEval {expression = "document.body || 'no document.body'"}
      logShow "Script evaluation result - DOM element or fallback" a11
      pause

      logTxt "Advanced Test 12: Window proxy evaluation"
      a12 <- scriptEvaluate $ baseEval {expression = "window"}
      logShow "Script evaluation result - window proxy" a12
      pause

      logTxt "Advanced Test 13: Map evaluation (ES6 collection)"
      a13 <- scriptEvaluate $ baseEval {expression = "new Map([['key1', 'value1'], ['key2', 42]])"}
      logShow "Script evaluation result - Map" a13
      pause

      logTxt "Advanced Test 14: Set evaluation (ES6 collection)"
      a14 <- scriptEvaluate $ baseEval {expression = "new Set([1, 2, 3, 1, 2])"}
      logShow "Script evaluation result - Set" a14
      pause

      logTxt "Advanced Test 15: WeakMap evaluation (ES6 collection)"
      a15 <- scriptEvaluate $ baseEval {expression = "new WeakMap()"}
      logShow "Script evaluation result - WeakMap" a15
      pause

      logTxt "Advanced Test 16: WeakSet evaluation (ES6 collection)"
      a16 <- scriptEvaluate $ baseEval {expression = "new WeakSet()"}
      logShow "Script evaluation result - WeakSet" a16
      pause

      logTxt "Advanced Test 17: Generator function evaluation"
      a17 <- scriptEvaluate $ baseEval {expression = "function* gen() { yield 1; yield 2; }"}
      logShow "Script evaluation result - generator function" a17
      pause

      logTxt "Advanced Test 18: Generator evaluation"
      a18 <- scriptEvaluate $ baseEval {expression = "(function* gen() { yield 1; yield 2; })()"}
      logShow "Script evaluation result - generator" a18
      pause

      logTxt "Advanced Test 19: Proxy evaluation"
      a19 <- scriptEvaluate $ baseEval {expression = "new Proxy({}, { get: (target, prop) => 'proxied: ' + prop })"}
      logShow "Script evaluation result - proxy" a19
      pause

      logTxt "Advanced Test 20: ArrayBuffer evaluation (typed arrays)"
      a20 <- scriptEvaluate $ baseEval {expression = "new ArrayBuffer(16)"}
      logShow "Script evaluation result - ArrayBuffer" a20
      pause

      logTxt "Advanced Test 21: Typed array evaluation (Uint8Array)"
      a21 <- scriptEvaluate $ baseEval {expression = "new Uint8Array([1, 2, 3, 4, 5])"}
      logShow "Script evaluation result - Uint8Array" a21
      pause

      logTxt "Advanced Test 22: Complex expression with mixed primitive types"
      a22 <- scriptEvaluate $ baseEval {expression = "({ str: 'text', num: 42, bool: true, nul: null, undef: undefined, big: 123n })"}
      logShow "Script evaluation result - object with all primitive types" a22
      pause

      logTxt "Advanced Test 23: Serialization options test - limited depth"
      a23 <-
        scriptEvaluate $
          baseEval
            { expression = "({ level1: { level2: { level3: { deep: 'value' } } } })",
              serializationOptions =
                Just $
                  MkSerializationOptions
                    { maxDomDepth = Just (Just (MkJSUInt 2)),
                      maxObjectDepth = Just (Just (MkJSUInt 1)),
                      includeShadowTree = Just ShadowTreeNone
                    }
            }
      logShow "Script evaluation result - limited serialization depth" a23
      pause

      logTxt "Advanced Test 24: Result ownership test"
      a24 <-
        scriptEvaluate $
          baseEval
            { expression = "({ data: 'for ownership test' })",
              resultOwnership = Just Root
            }
      logShow "Script evaluation result - with ownership" a24
      pause

      logTxt "Advanced Test 25: Sandbox evaluation"
      a25 <-
        scriptEvaluate $
          baseEval
            { expression = "typeof sandbox_test_var",
              target = ContextTarget $ MkContextTarget {context = bc, sandbox = Just $ MkSandbox "test-sandbox"}
            }
      logShow "Script evaluation result - in sandbox" a25
      pause

-- TODO: Move to test  - this is special because of nested maybe in type
-- >>> runDemo serializationOptionsDemo
serializationOptionsDemo :: BiDiDemo
serializationOptionsDemo =
  demo "Serialization Options - Various Configurations" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action MkDemoUtils {..} _bidi = do
      let logJSON hdr = log (hdr <> ":\n") . jsonToText . toJSON

      logJSON "JSON for Nothing serializationOptions" $
        MkSerializationOptions
          { maxDomDepth = Nothing,
            maxObjectDepth = Nothing,
            includeShadowTree = Nothing
          }
      pause

      logJSON "JSON for maxDomDepth serializationOptions" $
        MkSerializationOptions
          { maxDomDepth = Just (Just (MkJSUInt 3)),
            maxObjectDepth = Nothing,
            includeShadowTree = Nothing
          }
      pause

      logJSON "JSON for maxDomDepth Nothing serializationOptions" $
        MkSerializationOptions
          { maxDomDepth = Just Nothing,
            maxObjectDepth = Nothing,
            includeShadowTree = Nothing
          }
      pause

      logJSON "JSON for maxObjectDepth serializationOptions" $
        MkSerializationOptions
          { maxDomDepth = Nothing,
            maxObjectDepth = Just (Just (MkJSUInt 2)),
            includeShadowTree = Nothing
          }
      pause

      logJSON "JSON for maxObjectDepth Nothing serializationOptions" $
        MkSerializationOptions
          { maxDomDepth = Nothing,
            maxObjectDepth = Just Nothing,
            includeShadowTree = Nothing
          }
      pause

      logJSON "JSON for includeShadowTree None serializationOptions" $
        MkSerializationOptions
          { maxDomDepth = Nothing,
            maxObjectDepth = Nothing,
            includeShadowTree = Just ShadowTreeNone
          }
      pause

      logJSON "JSON for includeShadowTree Open serializationOptions" $
        MkSerializationOptions
          { maxDomDepth = Nothing,
            maxObjectDepth = Nothing,
            includeShadowTree = Just Open
          }
      pause

      logJSON "JSON for includeShadowTree All serializationOptions" $
        MkSerializationOptions
          { maxDomDepth = Nothing,
            maxObjectDepth = Nothing,
            includeShadowTree = Just All
          }
      pause

      logJSON "JSON for all options set serializationOptions" $
        MkSerializationOptions
          { maxDomDepth = Just (Just (MkJSUInt 5)),
            maxObjectDepth = Just (Just (MkJSUInt 3)),
            includeShadowTree = Just Open
          }
      pause

      logJSON "JSON for all options Nothing serializationOptions" $
        MkSerializationOptions
          { maxDomDepth = Just Nothing,
            maxObjectDepth = Just Nothing,
            includeShadowTree = Just ShadowTreeNone
          }
      pause

-- >>> runDemo scriptPreloadScriptDemo
scriptPreloadScriptDemo :: BiDiDemo
scriptPreloadScriptDemo =
  -- • functionDeclaration property - basic JavaScript function execution
  -- • contexts property - targeting specific browsing contexts vs all contexts
  -- • sandbox property - script isolation and sandboxing
  demo "Script I - Basic Preload Script Properties" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi
      let chkDOM = chkDomContains utils bidi bc

      logTxt "Navigate to a simple test page"
      navResult <- browsingContextNavigate $ MkNavigate {context = bc, url = "data:text/html,<html><head><title>Preload Script Test</title></head><body><h1>Test Page</h1><p id='content'>Original content</p><div id='preload-indicator'></div></body></html>", wait = Just Complete}
      logShow "Navigation result" navResult
      pause

      logTxt "Test 1: Basic preload script with functionDeclaration and contexts"
      preloadScript1 <-
        scriptAddPreloadScript $
          MkAddPreloadScript
            { functionDeclaration =
                "function() { \
                \  document.addEventListener('DOMContentLoaded', function() { \
                \    var indicator = document.getElementById('preload-indicator'); \
                \    if (indicator) { \
                \      indicator.innerHTML = '<p style=\"color: green; font-weight: bold;\">✓ Basic Preload Script executed!</p>'; \
                \    } \
                \    var content = document.getElementById('content'); \
                \    if (content) { \
                \      content.style.backgroundColor = 'lightblue'; \
                \      content.innerHTML = 'Content modified by basic preload script!'; \
                \    } \
                \  }); \
                \}",
              arguments = Nothing,
              userContexts = Nothing,
              contexts = Just [bc],
              sandbox = Nothing
            }
      logShow "Basic preload script added" preloadScript1
      pause

      logTxt "Test 2: Preload script with sandbox isolation"
      preloadScriptSandbox <-
        scriptAddPreloadScript $
          MkAddPreloadScript
            { functionDeclaration =
                "function() { \
                \  window.sandboxedVariable = 'This should be isolated in sandbox'; \
                \  document.addEventListener('DOMContentLoaded', function() { \
                \    var notice = document.createElement('div'); \
                \    notice.id = 'sandbox-notice'; \
                \    notice.style.cssText = 'background: yellow; padding: 10px; margin: 5px; border: 2px solid orange;'; \
                \    notice.innerHTML = '<strong>Sandboxed Script:</strong> Executed in isolated environment'; \
                \    document.body.appendChild(notice); \
                \  }); \
                \}",
              arguments = Nothing,
              userContexts = Nothing,
              contexts = Just [bc],
              sandbox = Just "isolated-sandbox"
            }
      logShow "Sandboxed preload script added" preloadScriptSandbox
      pause

      logTxt "Test 3: Preload script with all contexts (global scope)"
      preloadScript2 <-
        scriptAddPreloadScript $
          MkAddPreloadScript
            { functionDeclaration =
                "function() { \
                \  window.PRELOADED_DATA = { \
                \    message: 'Hello from global preload script!', \
                \    timestamp: new Date().toISOString() \
                \  }; \
                \  console.log('Global Preload Script: Added global data', window.PRELOADED_DATA); \
                \}",
              arguments = Nothing,
              userContexts = Nothing,
              contexts = Nothing, -- Apply to all contexts
              sandbox = Nothing
            }
      logShow "Global preload script added" preloadScript2
      pause

      logTxt "Test 4: Navigate to a new page to see all preload scripts in action"
      navResult2 <-
        browsingContextNavigate $
          MkNavigate
            { context = bc,
              url = "data:text/html,<html><head><title>New Page</title></head><body><h1>New Test Page</h1><p id='content'>Fresh content</p><div id='preload-indicator'></div><div id='data-display'></div></body></html>",
              wait = Just Complete
            }
      logShow "Navigation result to new page" navResult2
      pause

      -- Wait a bit for the preload scripts to execute

      logTxt "Test 5: Check if basic preload script executed (DOM modifications)"
      chkDOM "✓ Basic Preload Script executed!"
      chkDOM "Content modified by basic preload script!"
      pause

      logTxt "Test 6: Check if sandboxed script executed (isolated execution)"
      chkDOM "Sandboxed Script:"
      chkDOM "Executed in isolated environment"
      pause

      logTxt "Test 7: Check if global preload script executed (all contexts)"
      -- Check if global data was added by evaluating a script that outputs to DOM
      scriptEvaluateNoWait $
        MkEvaluate
          { expression =
              "if (window.PRELOADED_DATA) { \
              \  document.body.innerHTML += '<div id=\"global-data-check\">Global data present: ' + JSON.stringify(window.PRELOADED_DATA) + '</div>'; \
              \} else { \
              \  document.body.innerHTML += '<div id=\"global-data-check\">No global data found</div>'; \
              \}",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      chkDOM "Global data present:"
      chkDOM "Hello from global preload script!"
      pause

      logTxt "Test 8: Verify sandbox isolation by checking if sandboxed variables are isolated"
      scriptEvaluateNoWait $
        MkEvaluate
          { expression =
              "var isolationTest = 'main context variable'; \
              \if (typeof window.sandboxedVariable !== 'undefined') { \
              \  document.body.innerHTML += '<div>WARNING: Sandbox leak detected - sandboxedVariable accessible</div>'; \
              \} else { \
              \  document.body.innerHTML += '<div>✓ Sandbox isolation confirmed - sandboxedVariable not accessible</div>'; \
              \}",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      chkDOM "✓ Sandbox isolation confirmed"
      pause

-- >>> runDemo scriptPreloadScriptMultiContextDemo
scriptPreloadScriptMultiContextDemo :: BiDiDemo
scriptPreloadScriptMultiContextDemo =
  -- • contexts property - multiple browsing context management and targeting
  -- • scriptRemovePreloadScript - selective script removal and cleanup
  -- • Cross-context script behavior and isolation verification
  demo "Script II - Multi-Context and Cleanup" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi
      let chkDOM = chkDomContains utils bidi bc

      logTxt "Create a new browsing context to test multiple contexts behavior"
      newContext <- newWindowContext utils bidi

      logTxt "Add a preload script specific to the new context only"
      preloadScriptNewContext <-
        scriptAddPreloadScript $
          MkAddPreloadScript
            { functionDeclaration =
                "function() { \
                \  window.addEventListener('load', function() { \
                \    var body = document.body; \
                \    if (body) { \
                \      var notice = document.createElement('div'); \
                \      notice.style.cssText = 'position: fixed; top: 10px; right: 10px; background: pink; padding: 10px; border: 2px solid red; z-index: 9999;'; \
                \      notice.innerHTML = '<strong>New Context Script!</strong><br/>Only in new window'; \
                \      body.appendChild(notice); \
                \    } \
                \  }); \
                \}",
              arguments = Nothing,
              userContexts = Nothing,
              contexts = Just [newContext], -- Only apply to new context
              sandbox = Nothing
            }
      logShow "Context-specific preload script added" preloadScriptNewContext
      pause

      logTxt "Add a global preload script for comparison"
      globalScript <-
        scriptAddPreloadScript $
          MkAddPreloadScript
            { functionDeclaration =
                "function() { \
                \  window.GLOBAL_PRELOAD_DATA = { \
                \    message: 'Global script active', \
                \    timestamp: new Date().toISOString() \
                \  }; \
                \}",
              arguments = Nothing,
              userContexts = Nothing,
              contexts = Nothing, -- Apply to all contexts
              sandbox = Nothing
            }
      logShow "Global preload script added" globalScript
      pause

      logTxt "Navigate both contexts to see context-specific script behavior"
      navResult3 <- browsingContextNavigate $ MkNavigate {context = bc, url = "data:text/html,<html><head><title>Original Context</title></head><body><h1>Original Context Page</h1><p id='content'>Original context content</p><div id='preload-indicator'></div></body></html>", wait = Just Complete}
      logShow "Navigation result - original context" navResult3
      pause

      navResultNew <- browsingContextNavigate $ MkNavigate {context = newContext, url = "data:text/html,<html><head><title>New Context</title></head><body><h1>New Context Page</h1><p id='content'>New context content</p><div id='preload-indicator'></div></body></html>", wait = Just Complete}
      logShow "Navigation result - new context" navResultNew
      pause

      -- Wait for all preload scripts to execute
      pauseAtLeast $ 1500 * milliseconds

      logTxt "Verify original context has only global script (no context-specific script)"
      scriptEvaluateNoWait $
        MkEvaluate
          { expression =
              "if (window.GLOBAL_PRELOAD_DATA) { \
              \  document.body.innerHTML += '<div>Original context global data confirmed</div>'; \
              \}",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      chkDOM "Original context global data confirmed"
      pause

      logTxt "Check new context has both global and context-specific scripts"
      -- Switch to new context for validation
      browsingContextActivate $ MkActivate newContext
      pauseAtLeast $ 500 * milliseconds

      -- Check new context content using scripts that add to DOM
      scriptEvaluateNoWait $
        MkEvaluate
          { expression =
              "if (window.GLOBAL_PRELOAD_DATA) { \
              \  document.body.innerHTML += '<div>New context has global data</div>'; \
              \} else { \
              \  document.body.innerHTML += '<div>New context missing global data</div>'; \
              \} \
              \if (document.querySelector('div[style*=\"position: fixed\"]')) { \
              \  document.body.innerHTML += '<div>New context has fixed position element</div>'; \
              \}",
            target = ContextTarget $ MkContextTarget {context = newContext, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      pauseAtLeast $ 500 * milliseconds

      -- For new context validation, we need to get DOM content from the new context
      domContentNew <-
        scriptEvaluate $
          MkEvaluate
            { expression = "document.body ? document.body.innerText || document.body.textContent || '' : ''",
              target = ContextTarget $ MkContextTarget {context = newContext, sandbox = Nothing},
              awaitPromise = False,
              resultOwnership = Nothing,
              serializationOptions = Nothing
            }

      case domContentNew of
        EvaluateResultSuccess {result = PrimitiveValue (StringValue (MkStringValue newContextText))} -> do
          logTxt $ "New context DOM content: " <> newContextText
          if "New context has global data" `isInfixOf` newContextText
            then logTxt "✓ New context has global script effects"
            else logTxt "✗ New context missing global script effects"
          if "New Context Script!" `isInfixOf` newContextText
            then logTxt "✓ New context has context-specific script effects"
            else logTxt "✗ New context missing context-specific script effects"
        _ -> logTxt "Could not read new context DOM content"
      pause

      logTxt "Test selective script removal - Remove context-specific script"
      let MkAddPreloadScriptResult scriptNewContextId = preloadScriptNewContext
      removeResultNewContext <- scriptRemovePreloadScript $ MkRemovePreloadScript scriptNewContextId
      logShow "Removed new context preload script" removeResultNewContext
      pause

      logTxt "Navigate original context to verify global script still works"
      browsingContextActivate $ MkActivate bc
      pauseAtLeast $ 500 * milliseconds

      navResult4 <- browsingContextNavigate $ MkNavigate {context = bc, url = "data:text/html,<html><head><title>After Context Removal</title></head><body><h1>After Context Script Removal</h1><p id='content'>Content after context removal</p><div id='preload-indicator'></div></body></html>", wait = Just Complete}
      logShow "Navigation after context script removal" navResult4
      pause

      -- Wait for remaining preload scripts
      pauseAtLeast $ 1500 * milliseconds

      logTxt "Verify global script still works after context-specific removal"
      scriptEvaluateNoWait $
        MkEvaluate
          { expression =
              "if (window.GLOBAL_PRELOAD_DATA) { \
              \  document.body.innerHTML += '<div>Global script still active after context removal</div>'; \
              \}",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      chkDOM "Global script still active after context removal"
      pause

      logTxt "Remove global preload script for complete cleanup"
      let MkAddPreloadScriptResult globalScriptId = globalScript
      removeResultGlobal <- scriptRemovePreloadScript $ MkRemovePreloadScript globalScriptId
      logShow "Removed global preload script" removeResultGlobal
      pause

      logTxt "Final navigation to verify complete cleanup"
      navResult5 <- browsingContextNavigate $ MkNavigate {context = bc, url = "data:text/html,<html><head><title>Clean Page</title></head><body><h1>Clean Page - No Preload Scripts</h1><p id='content'>Clean content</p><div id='preload-indicator'></div></body></html>", wait = Just Complete}
      logShow "Final navigation - clean page" navResult5
      pause

      -- Wait to ensure no preload scripts run
      pauseAtLeast $ 1000 * milliseconds

      logTxt "Final verification - no preload script effects should be present"
      scriptEvaluateNoWait $
        MkEvaluate
          { expression =
              "if (!window.GLOBAL_PRELOAD_DATA && !document.querySelector('div[style*=\"position: fixed\"]') && !document.querySelector('#sandbox-notice')) { \
              \  document.body.innerHTML += '<div>✓ Complete cleanup confirmed: No preload effects</div>'; \
              \} else { \
              \  document.body.innerHTML += '<div>⚠ Warning: Some preload effects still detected</div>'; \
              \}",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }
      chkDOM "✓ Complete cleanup confirmed: No preload effects"
      pause

      logTxt "Cleanup - close the new context"
      closeContext utils bidi newContext

-- >>> runDemo scriptChannelArgumentDemo
scriptChannelArgumentDemo :: BiDiDemo
scriptChannelArgumentDemo =
  demo "Script III - Channel Argument Test" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi
      let chkDOM = chkDomContains utils bidi bc

      logTxt "Navigate to a simple test page for channel test"
      navResult <- browsingContextNavigate $ MkNavigate {context = bc, url = "data:text/html,<html><head><title>Channel Test</title></head><body><h1>Channel Test Page</h1><div id='output'></div></body></html>", wait = Just Complete}
      logShow "Navigation result" navResult
      pause

      logTxt "Create channel value for preload script arguments"
      let channelValue =
            MkChannelValue
              { value =
                  MkChannelProperties
                    { channel = Channel "test-channel",
                      serializationOptions = Nothing,
                      ownership = Nothing
                    }
              }

      logTxt "Add preload script with channel argument"
      preloadScriptWithChannel <-
        scriptAddPreloadScript $
          MkAddPreloadScript
            { functionDeclaration =
                "function(channelArg) { \
                \  console.log('Channel arg received:', channelArg); \
                \  window.addEventListener('DOMContentLoaded', function() { \
                \    var output = document.getElementById('output'); \
                \    if (output) { \
                \      var div = document.createElement('div'); \
                \      div.id = 'channel-result'; \
                \      div.style.cssText = 'background: lightgreen; padding: 10px; border: 2px solid green; margin: 5px;'; \
                \      if (channelArg) { \
                \        div.innerHTML = '<strong>✓ Channel Argument Success:</strong> Received channel object'; \
                \      } else { \
                \        div.innerHTML = '<strong>✗ Channel Argument Failed:</strong> No channel received'; \
                \      } \
                \      output.appendChild(div); \
                \    } \
                \  }); \
                \}",
              arguments = Just [channelValue],
              contexts = Just [bc],
              userContexts = Nothing,
              sandbox = Nothing
            }
      logShow "Preload script with channel added" preloadScriptWithChannel
      pause

      logTxt "Navigate to trigger preload script execution"
      navResult2 <- browsingContextNavigate $ MkNavigate {context = bc, url = "data:text/html,<html><head><title>Channel Test Execution</title></head><body><h1>Testing Channel Arguments</h1><div id='output'></div></body></html>", wait = Just Complete}
      logShow "Navigation result for execution" navResult2
      pause

      logTxt "Check if channel argument was received successfully"
      chkDOM "✓ Channel Argument Success"
      chkDOM "Received channel object"
      pause

      logTxt "Clean up - remove the channel preload script"
      let MkAddPreloadScriptResult scriptId = preloadScriptWithChannel
      removeResult <- scriptRemovePreloadScript $ MkRemovePreloadScript scriptId
      logShow "Removed channel preload script" removeResult
      pause

      logTxt "Final verification - navigate to clean page"
      navResultFinal <- browsingContextNavigate $ MkNavigate {context = bc, url = "data:text/html,<html><head><title>Clean Final</title></head><body><h1>Clean Page</h1><div id='output'></div></body></html>", wait = Just Complete}
      logShow "Final clean navigation" navResultFinal
      pause

      -- Check DOM is clean (no channel script effects
      domContent <-
        scriptEvaluate $
          MkEvaluate
            { expression = "document.body ? document.body.innerText || document.body.textContent || '' : ''",
              target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
              awaitPromise = False,
              resultOwnership = Nothing,
              serializationOptions = Nothing
            }

      case domContent of
        EvaluateResultSuccess {result = PrimitiveValue (StringValue (MkStringValue cleanText))} -> do
          if "Channel Argument Success" `isInfixOf` cleanText
            then logTxt "⚠ Warning: Channel script effects still present after removal"
            else logTxt "✓ Confirmed: Clean state - no channel script effects"
        _ -> logTxt "Could not verify clean state"
      pause

-- >>> runDemo scriptUserContextsDemo
scriptUserContextsDemo :: BiDiDemo
scriptUserContextsDemo =
  demo "Script IV - UserContexts Property Exclusive Demo" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      logTxt "Creating multiple user contexts to demonstrate userContexts property"

      logTxt "Create User Context 1 - isolated environment"
      userContext1 <-
        browserCreateUserContext
          MkCreateUserContext
            { insecureCerts = Just True,
              proxy = Nothing,
              unhandledPromptBehavior = Nothing
            }
      logShow "User Context 1 created" userContext1
      pause

      logTxt "Create User Context 2 - different configuration"
      userContext2 <-
        browserCreateUserContext
          MkCreateUserContext
            { insecureCerts = Just False,
              proxy = Nothing,
              unhandledPromptBehavior = Nothing
            }
      logShow "User Context 2 created" userContext2
      pause

      logTxt "Create User Context 3 - additional isolation"
      userContext3 <-
        browserCreateUserContext
          MkCreateUserContext
            { insecureCerts = Nothing,
              proxy = Nothing,
              unhandledPromptBehavior = Nothing
            }
      logShow "User Context 3 created" userContext3
      pause

      logTxt "Get all existing user contexts to verify creation"
      allUserContexts <- browserGetUserContexts
      logShow "All user contexts available" allUserContexts
      pause

      logTxt "Test 1: Add preload script targeting specific user contexts only"
      preloadScriptSpecificUsers <-
        scriptAddPreloadScript $
          MkAddPreloadScript
            { functionDeclaration =
                "function() { \
                \  window.USER_CONTEXT_SPECIFIC_DATA = { \
                \    message: 'This script targets specific user contexts only!', \
                \    timestamp: new Date().toISOString(), \
                \    source: 'userContexts-specific-script' \
                \  }; \
                \  document.addEventListener('DOMContentLoaded', function() { \
                \    var indicator = document.createElement('div'); \
                \    indicator.id = 'user-context-indicator'; \
                \    indicator.style.cssText = 'background: purple; color: white; padding: 15px; margin: 10px; border: 3px solid indigo; border-radius: 5px;'; \
                \    indicator.innerHTML = '<strong>✓ UserContexts Script Active!</strong><br/>Targeting specific user contexts: [UC1, UC2]'; \
                \    document.body.appendChild(indicator); \
                \  }); \
                \}",
              arguments = Nothing,
              contexts = Nothing, -- Apply to all browsing contexts
              userContexts = Just [userContext1, userContext2], -- Only these user contexts
              sandbox = Nothing
            }
      logShow "UserContexts-specific preload script added" preloadScriptSpecificUsers
      pause

      logTxt "Test 2: Add preload script with no userContexts restriction (global)"
      preloadScriptGlobal <-
        scriptAddPreloadScript $
          MkAddPreloadScript
            { functionDeclaration =
                "function() { \
                \  window.GLOBAL_USER_CONTEXT_DATA = { \
                \    message: 'Global script - all user contexts', \
                \    timestamp: new Date().toISOString() \
                \  }; \
                \  document.addEventListener('DOMContentLoaded', function() { \
                \    var indicator = document.createElement('div'); \
                \    indicator.id = 'global-indicator'; \
                \    indicator.style.cssText = 'background: gray; color: white; padding: 10px; margin: 5px; border: 2px solid black;'; \
                \    indicator.innerHTML = '<strong>Global Script Active</strong><br/>No userContexts restriction'; \
                \    document.body.appendChild(indicator); \
                \  }); \
                \}",
              arguments = Nothing,
              contexts = Nothing,
              userContexts = Nothing, -- Apply to all user contexts
              sandbox = Nothing
            }
      logShow "Global (no userContexts restriction) preload script added" preloadScriptGlobal
      pause

      logTxt "Test 3: Add preload script targeting only UserContext3"
      preloadScriptSingleUser <-
        scriptAddPreloadScript $
          MkAddPreloadScript
            { functionDeclaration =
                "function() { \
                \  window.SINGLE_USER_CONTEXT_DATA = { \
                \    message: 'Single user context script - UC3 only!', \
                \    userContext: 'UC3', \
                \    timestamp: new Date().toISOString() \
                \  }; \
                \  document.addEventListener('DOMContentLoaded', function() { \
                \    var indicator = document.createElement('div'); \
                \    indicator.id = 'single-user-indicator'; \
                \    indicator.style.cssText = 'background: orange; color: black; padding: 12px; margin: 8px; border: 2px solid darkorange; font-weight: bold;'; \
                \    indicator.innerHTML = '<strong>✓ Single UserContext Script!</strong><br/>Only UserContext3 targeted'; \
                \    document.body.appendChild(indicator); \
                \  }); \
                \}",
              arguments = Nothing,
              contexts = Nothing,
              userContexts = Just [userContext3], -- Only userContext3
              sandbox = Nothing
            }
      logShow "Single UserContext (UC3) preload script added" preloadScriptSingleUser
      pause

      logTxt "Create browsing contexts in different user contexts to test script targeting"

      logTxt "Create browsing context in UserContext1"
      bcUserContext1 <-
        browsingContextCreate $
          MkCreate
            { createType = Tab,
              background = False,
              referenceContext = Nothing,
              userContext = Just userContext1
            }
      logShow "Browsing context in UserContext1" bcUserContext1
      pause

      logTxt "Create browsing context in UserContext2"
      bcUserContext2 <-
        browsingContextCreate $
          MkCreate
            { createType = Tab,
              background = False,
              referenceContext = Nothing,
              userContext = Just userContext2
            }
      logShow "Browsing context in UserContext2" bcUserContext2
      pause

      logTxt "Create browsing context in UserContext3"
      bcUserContext3 <-
        browsingContextCreate $
          MkCreate
            { createType = Tab,
              background = False,
              referenceContext = Nothing,
              userContext = Just userContext3
            }
      logShow "Browsing context in UserContext3" bcUserContext3
      pause

      logTxt "Test script execution in UserContext1 - should have specific AND global scripts"
      browsingContextActivate $ MkActivate bcUserContext1
      pauseAtLeast $ 500 * milliseconds

      navResultUC1 <- browsingContextNavigate $ MkNavigate {context = bcUserContext1, url = "data:text/html,<html><head><title>UserContext1 Test</title></head><body><h1>UserContext1 Page</h1><div id='content'>Testing UserContext1</div></body></html>", wait = Just Complete}
      logShow "Navigation in UserContext1" navResultUC1
      pauseAtLeast $ 1500 * milliseconds -- Wait for preload scripts
      logTxt "Verify UserContext1 has both global and specific scripts"
      scriptEvaluateNoWait $
        MkEvaluate
          { expression =
              "var results = []; \
              \if (window.GLOBAL_USER_CONTEXT_DATA) results.push('Global script active'); \
              \if (window.USER_CONTEXT_SPECIFIC_DATA) results.push('Specific script active'); \
              \if (window.SINGLE_USER_CONTEXT_DATA) results.push('Single script active'); \
              \document.body.innerHTML += '<div id=\"uc1-results\">UC1 Results: ' + results.join(', ') + '</div>';",
            target = ContextTarget $ MkContextTarget {context = bcUserContext1, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }

      -- Check UserContext1 content
      domContentUC1 <-
        scriptEvaluate $
          MkEvaluate
            { expression = "document.body ? document.body.innerText || document.body.textContent || '' : ''",
              target = ContextTarget $ MkContextTarget {context = bcUserContext1, sandbox = Nothing},
              awaitPromise = False,
              resultOwnership = Nothing,
              serializationOptions = Nothing
            }

      case domContentUC1 of
        EvaluateResultSuccess {result = PrimitiveValue (StringValue (MkStringValue uc1Text))} -> do
          logTxt $ "UserContext1 content: " <> uc1Text
          if "Specific script active" `isInfixOf` uc1Text
            then logTxt "✓ UserContext1: Specific userContexts script executed"
            else logTxt "✗ UserContext1: Missing specific userContexts script"
          if "Global script active" `isInfixOf` uc1Text
            then logTxt "✓ UserContext1: Global script executed"
            else logTxt "✗ UserContext1: Missing global script"
          if "Single script active" `isInfixOf` uc1Text
            then logTxt "✗ UserContext1: Unexpected single UC3 script execution"
            else logTxt "✓ UserContext1: Correctly excluded single UC3 script"
        _ -> logTxt "Could not read UserContext1 content"
      pause

      logTxt "Test script execution in UserContext2 - should have specific AND global scripts"
      browsingContextActivate $ MkActivate bcUserContext2
      pauseAtLeast $ 500 * milliseconds

      navResultUC2 <- browsingContextNavigate $ MkNavigate {context = bcUserContext2, url = "data:text/html,<html><head><title>UserContext2 Test</title></head><body><h1>UserContext2 Page</h1><div id='content'>Testing UserContext2</div></body></html>", wait = Just Complete}
      logShow "Navigation in UserContext2" navResultUC2
      pauseAtLeast $ 1500 * milliseconds -- Wait for preload scripts
      logTxt "Verify UserContext2 has both global and specific scripts"
      scriptEvaluateNoWait $
        MkEvaluate
          { expression =
              "var results = []; \
              \if (window.GLOBAL_USER_CONTEXT_DATA) results.push('Global script active'); \
              \if (window.USER_CONTEXT_SPECIFIC_DATA) results.push('Specific script active'); \
              \if (window.SINGLE_USER_CONTEXT_DATA) results.push('Single script active'); \
              \document.body.innerHTML += '<div id=\"uc2-results\">UC2 Results: ' + results.join(', ') + '</div>';",
            target = ContextTarget $ MkContextTarget {context = bcUserContext2, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }

      -- Check UserContext2 content
      domContentUC2 <-
        scriptEvaluate $
          MkEvaluate
            { expression = "document.body ? document.body.innerText || document.body.textContent || '' : ''",
              target = ContextTarget $ MkContextTarget {context = bcUserContext2, sandbox = Nothing},
              awaitPromise = False,
              resultOwnership = Nothing,
              serializationOptions = Nothing
            }

      case domContentUC2 of
        EvaluateResultSuccess {result = PrimitiveValue (StringValue (MkStringValue uc2Text))} -> do
          logTxt $ "UserContext2 content: " <> uc2Text
          if "Specific script active" `isInfixOf` uc2Text
            then logTxt "✓ UserContext2: Specific userContexts script executed"
            else logTxt "✗ UserContext2: Missing specific userContexts script"
          if "Global script active" `isInfixOf` uc2Text
            then logTxt "✓ UserContext2: Global script executed"
            else logTxt "✗ UserContext2: Missing global script"
          if "Single script active" `isInfixOf` uc2Text
            then logTxt "✗ UserContext2: Unexpected single UC3 script execution"
            else logTxt "✓ UserContext2: Correctly excluded single UC3 script"
        _ -> logTxt "Could not read UserContext2 content"
      pause

      logTxt "Test script execution in UserContext3 - should have global AND single scripts, NOT specific"
      browsingContextActivate $ MkActivate bcUserContext3
      pauseAtLeast $ 500 * milliseconds

      navResultUC3 <- browsingContextNavigate $ MkNavigate {context = bcUserContext3, url = "data:text/html,<html><head><title>UserContext3 Test</title></head><body><h1>UserContext3 Page</h1><div id='content'>Testing UserContext3</div></body></html>", wait = Just Complete}
      logShow "Navigation in UserContext3" navResultUC3
      pauseAtLeast $ 1500 * milliseconds -- Wait for preload scripts
      logTxt "Verify UserContext3 has global and single scripts, but NOT specific"
      scriptEvaluateNoWait $
        MkEvaluate
          { expression =
              "var results = []; \
              \if (window.GLOBAL_USER_CONTEXT_DATA) results.push('Global script active'); \
              \if (window.USER_CONTEXT_SPECIFIC_DATA) results.push('Specific script active'); \
              \if (window.SINGLE_USER_CONTEXT_DATA) results.push('Single script active'); \
              \document.body.innerHTML += '<div id=\"uc3-results\">UC3 Results: ' + results.join(', ') + '</div>';",
            target = ContextTarget $ MkContextTarget {context = bcUserContext3, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }

      -- Check UserContext3 content
      domContentUC3 <-
        scriptEvaluate $
          MkEvaluate
            { expression = "document.body ? document.body.innerText || document.body.textContent || '' : ''",
              target = ContextTarget $ MkContextTarget {context = bcUserContext3, sandbox = Nothing},
              awaitPromise = False,
              resultOwnership = Nothing,
              serializationOptions = Nothing
            }

      case domContentUC3 of
        EvaluateResultSuccess {result = PrimitiveValue (StringValue (MkStringValue uc3Text))} -> do
          logTxt $ "UserContext3 content: " <> uc3Text
          if "Specific script active" `isInfixOf` uc3Text
            then logTxt "✗ UserContext3: Unexpected specific userContexts script execution"
            else logTxt "✓ UserContext3: Correctly excluded specific userContexts script (UC1,UC2 only)"
          if "Global script active" `isInfixOf` uc3Text
            then logTxt "✓ UserContext3: Global script executed"
            else logTxt "✗ UserContext3: Missing global script"
          if "Single script active" `isInfixOf` uc3Text
            then logTxt "✓ UserContext3: Single UC3 script executed correctly"
            else logTxt "✗ UserContext3: Missing single UC3 script"
        _ -> logTxt "Could not read UserContext3 content"
      pause

      logTxt "Test original browsing context (default user context) - should only have global script"
      browsingContextActivate $ MkActivate bc
      pauseAtLeast $ 500 * milliseconds

      navResultOriginal <- browsingContextNavigate $ MkNavigate {context = bc, url = "data:text/html,<html><head><title>Original Context Test</title></head><body><h1>Original Context Page</h1><div id='content'>Testing Original Default Context</div></body></html>", wait = Just Complete}
      logShow "Navigation in original context" navResultOriginal
      pauseAtLeast $ 1500 * milliseconds -- Wait for preload scripts
      logTxt "Verify original context has only global script (no user context restrictions)"
      scriptEvaluateNoWait $
        MkEvaluate
          { expression =
              "var results = []; \
              \if (window.GLOBAL_USER_CONTEXT_DATA) results.push('Global script active'); \
              \if (window.USER_CONTEXT_SPECIFIC_DATA) results.push('Specific script active'); \
              \if (window.SINGLE_USER_CONTEXT_DATA) results.push('Single script active'); \
              \document.body.innerHTML += '<div id=\"original-results\">Original Results: ' + results.join(', ') + '</div>';",
            target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
            awaitPromise = False,
            resultOwnership = Nothing,
            serializationOptions = Nothing
          }

      -- Check original context content
      domContentOriginal <-
        scriptEvaluate $
          MkEvaluate
            { expression = "document.body ? document.body.innerText || document.body.textContent || '' : ''",
              target = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing},
              awaitPromise = False,
              resultOwnership = Nothing,
              serializationOptions = Nothing
            }

      case domContentOriginal of
        EvaluateResultSuccess {result = PrimitiveValue (StringValue (MkStringValue originalText))} -> do
          logTxt $ "Original context content: " <> originalText
          if "Specific script active" `isInfixOf` originalText
            then logTxt "✗ Original context: Unexpected specific userContexts script execution"
            else logTxt "✓ Original context: Correctly excluded specific userContexts script"
          if "Global script active" `isInfixOf` originalText
            then logTxt "✓ Original context: Global script executed"
            else logTxt "✗ Original context: Missing global script"
          if "Single script active" `isInfixOf` originalText
            then logTxt "✗ Original context: Unexpected single UC3 script execution"
            else logTxt "✓ Original context: Correctly excluded single UC3 script"
        _ -> logTxt "Could not read original context content"
      pause

      logTxt "Summary of userContexts property demonstration:"
      logTxt "• userContexts = Just [UC1, UC2]: Script executes only in those user contexts"
      logTxt "• userContexts = Just [UC3]: Script executes only in UC3"
      logTxt "• userContexts = Nothing: Script executes in all user contexts (global)"
      logTxt "• Default context behavior: Only receives scripts with userContexts = Nothing"
      pause

      logTxt "Cleanup - Remove all preload scripts"
      let MkAddPreloadScriptResult scriptSpecificId = preloadScriptSpecificUsers
      let MkAddPreloadScriptResult scriptGlobalId = preloadScriptGlobal
      let MkAddPreloadScriptResult scriptSingleId = preloadScriptSingleUser

      removeSpecific <- scriptRemovePreloadScript $ MkRemovePreloadScript scriptSpecificId
      logShow "Removed specific userContexts script" removeSpecific
      removeGlobal <- scriptRemovePreloadScript $ MkRemovePreloadScript scriptGlobalId
      logShow "Removed global script" removeGlobal
      removeSingle <- scriptRemovePreloadScript $ MkRemovePreloadScript scriptSingleId
      logShow "Removed single userContext script" removeSingle
      pause

      logTxt "Cleanup - Close user context browsing contexts"
      closeContext utils bidi bcUserContext1
      closeContext utils bidi bcUserContext2
      closeContext utils bidi bcUserContext3
      pause

      logTxt "Cleanup - Remove user contexts"
      removeUC1 <- browserRemoveUserContext $ MkRemoveUserContext userContext1
      logShow "Removed UserContext1" removeUC1
      removeUC2 <- browserRemoveUserContext $ MkRemoveUserContext userContext2
      logShow "Removed UserContext2" removeUC2
      removeUC3 <- browserRemoveUserContext $ MkRemoveUserContext userContext3
      logShow "Removed UserContext3" removeUC3
      pause

      logTxt "Final verification - check remaining user contexts"
      finalUserContexts <- browserGetUserContexts
      logShow "Remaining user contexts after cleanup" finalUserContexts
      pause

-- >>> runDemo scriptCallFunctionDemo
scriptCallFunctionDemo :: BiDiDemo
scriptCallFunctionDemo =
  demo "Script V - script.callFunction Core Scenarios" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      logTxt "Navigate to a simple page for function call tests"
      navResult <-
        browsingContextNavigate $
          MkNavigate
            { context = bc,
              url = "data:text/html,<html><head><title>CallFunction Demo</title></head><body><h1>CallFunction Demo</h1><div id='output'></div></body></html>",
              wait = Just Complete
            }
      logShow "Navigation result" navResult
      pause

      let baseTarget = ContextTarget $ MkContextTarget {context = bc, sandbox = Nothing}

      -- Test 1: Basic synchronous function returning a string
      logTxt "Test 1: Basic synchronous function (string result)"
      r1 <-
        scriptCallFunction $
          MkCallFunction
            { functionDeclaration = "function() { return 'Hello from callFunction!'; }",
              awaitPromise = False,
              target = baseTarget,
              arguments = Nothing,
              resultOwnership = Nothing,
              serializationOptions = Nothing,
              this = Nothing
            }
      logShow "callFunction result - basic string" r1
      pause

      -- Test 2: Function with arguments (numbers) and arithmetic
      logTxt "Test 2: Function with numeric arguments"
      let numArg n = PrimitiveLocalValue (NumberValue (Left n))
      r2 <-
        scriptCallFunction $
          MkCallFunction
            { functionDeclaration = "function(a, b) { return a + b + 0.5; }",
              awaitPromise = False,
              target = baseTarget,
              arguments = Just [numArg 41, numArg 1],
              resultOwnership = Nothing,
              serializationOptions = Nothing,
              this = Nothing
            }
      logShow "callFunction result - arithmetic" r2
      pause

      -- Test 3: Promise-returning function with awaitPromise=True
      logTxt "Test 3: Promise-returning function (awaitPromise=True)"
      r3 <-
        scriptCallFunction $
          MkCallFunction
            { functionDeclaration = "async function(name) { return new Promise(r => setTimeout(()=> r('Hi ' + name), 50)); }",
              awaitPromise = True,
              target = baseTarget,
              arguments = Just [PrimitiveLocalValue (StringValue (MkStringValue "BiDi"))],
              resultOwnership = Nothing,
              serializationOptions = Nothing,
              this = Nothing
            }
      logShow "callFunction result - awaited promise" r3
      pause

      -- Test 4: Function throwing an error (exception path)
      logTxt "Test 4: Function throwing an exception"
      r4 <-
        scriptCallFunction $
          MkCallFunction
            { functionDeclaration = "function() { throw new Error('Boom from callFunction'); }",
              awaitPromise = False,
              target = baseTarget,
              arguments = Nothing,
              resultOwnership = Nothing,
              serializationOptions = Nothing,
              this = Nothing
            }
      logShow "callFunction result - exception" r4
      pause

      -- Test 5: Function with resultOwnership = Root to obtain a handle (if remote value)
      logTxt "Test 5: Object result with ownership=Root"
      r5 <-
        scriptCallFunction $
          MkCallFunction
            { functionDeclaration = "function() { return { message: 'Owned object', time: Date.now() }; }",
              awaitPromise = False,
              target = baseTarget,
              arguments = Nothing,
              resultOwnership = Just Root,
              serializationOptions = Nothing,
              this = Nothing
            }
      logShow "callFunction result - owned object" r5
      pause

      -- Test 6: Function with serializationOptions limiting object depth
      logTxt "Test 6: serializationOptions (maxObjectDepth=1)"
      r6 <-
        scriptCallFunction $
          MkCallFunction
            { functionDeclaration = "function() { return { level1: { level2: { value: 42 } }, keep: 'yes' }; }",
              awaitPromise = False,
              target = baseTarget,
              arguments = Nothing,
              resultOwnership = Nothing,
              serializationOptions =
                Just
                  MkSerializationOptions
                    { maxDomDepth = Nothing,
                      maxObjectDepth = Just (Just (MkJSUInt 1)),
                      includeShadowTree = Just ShadowTreeNone
                    },
              this = Nothing
            }
      logShow "callFunction result - limited serialization" r6
      pause

      -- Test 7: Using 'this' binding (object as this) and argument to access property
      logTxt "Test 7: Using 'this' binding to access property"
      let objLocal =
            ObjectLocalValue
              MkObjectLocalValue
                { value =
                    MkMappingLocalValue
                      [ (Right "greeting", PrimitiveLocalValue (StringValue (MkStringValue "Hello"))),
                        (Right "name", PrimitiveLocalValue (StringValue (MkStringValue "World")))
                      ]
                }
      r7 <-
        scriptCallFunction $
          MkCallFunction
            { functionDeclaration = "function(extra) { return this.greeting + ', ' + this.name + extra; }",
              awaitPromise = False,
              target = baseTarget,
              arguments = Just [PrimitiveLocalValue (StringValue (MkStringValue "!!!"))],
              resultOwnership = Nothing,
              serializationOptions = Nothing,
              this = Just objLocal
            }
      logShow "callFunction result - this binding" r7
      pause

      logTxt "script.callFunction demo complete"
      pause

-- >>> runDemo scriptGetRealmsAndDisownDemo
scriptGetRealmsAndDisownDemo :: BiDiDemo
scriptGetRealmsAndDisownDemo =
  demo "Script VI - getRealms and disown Integration" action
  where
    action :: DemoUtils -> BiDiActions -> IO ()
    action utils@MkDemoUtils {..} bidi@MkBiDiActions {..} = do
      bc <- rootContext utils bidi

      logTxt "Navigate to a test page for realms and ownership demo"
      navResult <-
        browsingContextNavigate $
          MkNavigate
            { context = bc,
              url = "data:text/html,<html><head><title>Realms and Ownership Demo</title></head><body><h1>Realms and Ownership Demo</h1><div id='output'></div></body></html>",
              wait = Just Complete
            }
      logShow "Navigation result" navResult
      pause

      logTxt "Test 1: Get all realms without filtering"
      allRealms <-
        scriptGetRealms $
          MkGetRealms
            { context = Nothing, -- All contexts
              realmType = Nothing -- All realm types
            }
      logShow "All available realms" allRealms
      pause

      logTxt "Test 2: Get realms for specific browsing context"
      contextRealms <-
        scriptGetRealms $
          MkGetRealms
            { context = Just bc,
              realmType = Nothing
            }
      logShow "Realms for current browsing context" contextRealms
      pause

      logTxt "Test 3: Get only window realms"
      windowRealms <-
        scriptGetRealms $
          MkGetRealms
            { context = Nothing,
              realmType = Just Script.WindowRealm
            }
      logShow "Window realms only" windowRealms
      pause

      -- Extract the first available realm for subsequent tests
      let MkGetRealmsResult realms = allRealms
      case realms of
        [] -> logTxt "No realms available for ownership testing"
        (firstRealmInfo : _) -> do
          let targetRealm = case firstRealmInfo of
                Script.Window {base = Script.BaseRealmInfo {realm = r}} -> r
                Script.DedicatedWorker {base = Script.BaseRealmInfo {realm = r}} -> r
                Script.SharedWorker {base = Script.BaseRealmInfo {realm = r}} -> r
                Script.ServiceWorker {base = Script.BaseRealmInfo {realm = r}} -> r
                Script.Worker {base = Script.BaseRealmInfo {realm = r}} -> r
                Script.PaintWorklet {base = Script.BaseRealmInfo {realm = r}} -> r
                Script.AudioWorklet {base = Script.BaseRealmInfo {realm = r}} -> r
                Script.Worklet {base = Script.BaseRealmInfo {realm = r}} -> r

          logTxt $ "Test 4: Create owned objects in realm: " <> pack (show targetRealm)

          logTxt "Create first owned object (array)"
          ownedArray <-
            scriptEvaluate $
              MkEvaluate
                { expression = "[1, 2, 3, 'owned', { nested: 'data' }]",
                  target = RealmTarget targetRealm,
                  awaitPromise = False,
                  resultOwnership = Just Root,
                  serializationOptions = Nothing
                }
          logShow "Owned array result" ownedArray
          pause

          logTxt "Create second owned object (function)"
          ownedFunction <-
            scriptEvaluate $
              MkEvaluate
                { expression = "function ownedFunc() { return 'I am owned!'; }",
                  target = RealmTarget targetRealm,
                  awaitPromise = False,
                  resultOwnership = Just Root,
                  serializationOptions = Nothing
                }
          logShow "Owned function result" ownedFunction
          pause

          logTxt "Create third owned object (complex object)"
          ownedObject <-
            scriptEvaluate $
              MkEvaluate
                { expression = "({ id: Math.random(), message: 'Complex owned object', timestamp: Date.now(), data: [1,2,3] })",
                  target = RealmTarget targetRealm,
                  awaitPromise = False,
                  resultOwnership = Just Root,
                  serializationOptions = Nothing
                }
          logShow "Owned object result" ownedObject
          pause

          -- Extract handles from the evaluation results
          let extractHandle = \case
                EvaluateResultSuccess {result = rv} -> case rv of
                  ArrayValue {handle = Just h} -> Just h
                  ObjectValue {handle = Just h} -> Just h
                  FunctionValue {handle = Just h} -> Just h
                  SymbolValue {handle = Just h} -> Just h
                  RegExpValue {handle = Just h} -> Just h
                  DateValue {handle = Just h} -> Just h
                  MapValue {handle = Just h} -> Just h
                  SetValue {handle = Just h} -> Just h
                  WeakMapValue {handle = Just h} -> Just h
                  WeakSetValue {handle = Just h} -> Just h
                  GeneratorValue {handle = Just h} -> Just h
                  ErrorValue {handle = Just h} -> Just h
                  ProxyValue {handle = Just h} -> Just h
                  PromiseValue {handle = Just h} -> Just h
                  TypedArrayValue {handle = Just h} -> Just h
                  ArrayBufferValue {handle = Just h} -> Just h
                  NodeListValue {handle = Just h} -> Just h
                  HTMLCollectionValue {handle = Just h} -> Just h
                  WindowProxyValue {handle = Just h} -> Just h
                  _ -> Nothing
                _ -> Nothing

          let handles = catMaybes [extractHandle ownedArray, extractHandle ownedFunction, extractHandle ownedObject]

          if null handles
            then logTxt "No handles available for disown testing"
            else do
              logTxt $ "Test 5: Disown " <> pack (show (length handles)) <> " handles using scriptDisown"
              logShow "Handles to disown" handles

              disownResult <-
                scriptDisown $
                  MkDisown
                    { handles = handles,
                      target = RealmTarget targetRealm
                    }
              logShow "Disown operation result" disownResult
              pause

              logTxt "Test 6: Verification of disown operation complete"
              logTxt "Note: Disowned handles should no longer be accessible from the script context"
              pause

          logTxt "Test 7: Get realms again to ensure realm is still active"
          finalRealms <-
            scriptGetRealms $
              MkGetRealms
                { context = Just bc,
                  realmType = Just Script.WindowRealm
                }
          logShow "Final realms check" finalRealms
          pause

          logTxt "Demonstration complete: getRealms discovered realms, objects were created with ownership, and scriptDisown released the handles"
      pause
