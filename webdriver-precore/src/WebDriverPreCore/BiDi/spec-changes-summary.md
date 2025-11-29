# WebDriver BiDi Spec Changes Summary

**From:** July 18, 2025 → **To:** November 6, 2025

Focus: Client library implementation changes, particularly CDDL Index modifications

## CDDL Index Changes

### ResultData Group
**Added:**
- `BrowserResult` (new result group)
- `EmulationResult` (new result group)
- `InputResult` (new result group)

**Previously:** Only had `EmptyResult`, individual module results were not grouped

`NA - is void`

### Session Module

#### session.UnsubscribeByAttributesRequest
**Removed:** `? contexts: [+browsingContext.BrowsingContext]` parameter (deprecated functionality)

`NA - done already`

#### SessionResult
**Added result types:**
- `session.EndResult`
- `session.UnsubscribeResult`

`NA - is void`



### Browser Module

#### BrowserCommand
**Added:** `browser.SetDownloadBehavior`

#### BrowserResult (NEW)
**Added result types:**
- `browser.CloseResult` ~ NA empty result
- `browser.CreateUserContextResult` ~ NA alias
- `browser.GetClientWindowsResult`  done already
- `browser.GetUserContextsResult` done already
- `browser.RemoveUserContextResult` ~ NA empty result
- `browser.SetClientWindowStateResult` NA alais
- `browser.SetDownloadBehaviorResult`  NA empty result


#### browser.SetDownloadBehavior (NEW Command)
```cddl
browser.SetDownloadBehavior = (
  method: "browser.setDownloadBehavior",
  params: browser.SetDownloadBehaviorParameters
)

browser.SetDownloadBehaviorParameters = {
  downloadBehavior: browser.DownloadBehavior / null,
  ? userContexts: [+browser.UserContext]
}

browser.DownloadBehavior = {
  (
    browser.DownloadBehaviorAllowed //
    browser.DownloadBehaviorDenied
  )
}

browser.DownloadBehaviorAllowed = (
  type: "allowed",
  destinationFolder: text
)

browser.DownloadBehaviorDenied = (
  type: "denied"
)
```

### Emulation Module

#### EmulationCommand
**Added 4 new commands:**
- `emulation.SetForcedColorsModeThemeOverride`
- `emulation.SetNetworkConditions`
- `emulation.SetUserAgentOverride`
- `emulation.SetScriptingEnabled`

**Reordered:** Commands now alphabetically sorted

#### emulation.SetForcedColorsModeThemeOverride (NEW)
```cddl
emulation.SetForcedColorsModeThemeOverride = (
  method: "emulation.setForcedColorsModeThemeOverride",
  params: emulation.SetForcedColorsModeThemeOverrideParameters
)

emulation.SetForcedColorsModeThemeOverrideParameters = {
  theme: emulation.ForcedColorsModeTheme / null,
  ? contexts: [+browsingContext.BrowsingContext],
  ? userContexts: [+browser.UserContext],
}

emulation.ForcedColorsModeTheme = "light" / "dark"
```

#### emulation.SetNetworkConditions (NEW)
```cddl
emulation.SetNetworkConditions = (
  method: "emulation.setNetworkConditions",
  params: emulation.setNetworkConditionsParameters
)

emulation.setNetworkConditionsParameters = {
  networkConditions: emulation.NetworkConditions / null,
  ? contexts: [+browsingContext.BrowsingContext],
  ? userContexts: [+browser.UserContext],
}

emulation.NetworkConditions = emulation.NetworkConditionsOffline

emulation.NetworkConditionsOffline = {
  type: "offline"
}
```

#### emulation.SetUserAgentOverride (NEW)
```cddl
emulation.SetUserAgentOverride = (
  method: "emulation.setUserAgentOverride",
  params: emulation.SetUserAgentOverrideParameters
)

emulation.SetUserAgentOverrideParameters = {
  userAgent: text / null,
  ? contexts: [+browsingContext.BrowsingContext],
  ? userContexts: [+browser.UserContext],
}
```

#### emulation.SetScriptingEnabled (NEW)
```cddl
emulation.SetScriptingEnabled = (
  method: "emulation.setScriptingEnabled",
  params: emulation.SetScriptingEnabledParameters
)

emulation.SetScriptingEnabledParameters = {
  enabled: false / null,
  ? contexts: [+browsingContext.BrowsingContext],
  ? userContexts: [+browser.UserContext],
}
```


### Network Module

#### NetworkCommand
**Added:** `network.SetExtraHeaders`

#### network.DataType
**Changed:** `"response"` → `"request" / "response"` (added request data support)


#### network.SetExtraHeaders (NEW)
```cddl
network.SetExtraHeaders = (
  method: "network.setExtraHeaders",
  params: network.SetExtraHeadersParameters
)

network.SetExtraHeadersParameters = {
  headers: [*network.Header]
  ? contexts: [+browsingContext.BrowsingContext]
  ? userContexts: [+browser.UserContext]
}
```

--- 

### Input Module

-- NA All void

#### InputResult (NEW Group)
```cddl
InputResult = (
  input.PerformActionsResult /
  input.ReleaseActionsResult /
  input.SetFilesResult
)
```

### Browsing Context Module

#### browsingContext.LocateNodesParameters
**Removed:** `with` suffix (was malformed in old spec)

#### browsingContext.NavigateParameters
**Removed:** `with` suffix (was malformed in old spec)

#### script.NodeRemoteValue
**Removed:** `with` prefix (was malformed in old spec)

## Summary for Implementation

### High Priority Changes
1. **Emulation**: Add 4 new commands (forced colors, network conditions, user agent, scripting)
2. **Browser**: Add download behavior command
3. **Network**: Add extra headers command + request data type
4. **Session**: Remove deprecated contexts from unsubscribe

### Type System Changes
- All command results now properly grouped under `ResultData`
- Better separation between browser/emulation/input results

### Deprecation
- `session.unsubscribe` contexts parameter removed from CDDL (deprecated feature)


-- Update spec again
