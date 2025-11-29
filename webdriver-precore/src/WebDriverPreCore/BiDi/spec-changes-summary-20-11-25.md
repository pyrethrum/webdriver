# WebDriver BiDi Spec Changes Summary

**From:** November 6, 2025 → **To:** November 20, 2025

Focus: Emulation module enhancements with screen settings override

## Summary

This update adds a new screen settings override command to the Emulation module and renames a session parameter type for consistency.

---

## Session Module

### session.SubscriptionRequest Type Rename

**Changed:** `session.SubscriptionRequest` → `session.SubscribeParameters`

The type name was updated to better reflect its purpose as parameters for the subscribe command specifically (rather than being used for both subscribe and unsubscribe).

#### CDDL

```cddl
session.SubscribeParameters = {
  events: [*text],
  ? contexts: [*browsingContext.BrowsingContext],
}
```

**Previous name:**
```cddl
session.SubscriptionRequest = {
  events: [*text],
  ? contexts: [*browsingContext.BrowsingContext],
}
```

**Usage:**
```cddl
session.Subscribe = (
  method: "session.subscribe",
  params: session.SubscribeParameters
)
```

## Emulation Module

### emulation.setScreenSettingsOverride Command (NEW)

A new command to emulate web-exposed screen area and web-exposed available screen area for top-level traversables or user contexts.

#### CDDL

```cddl
emulation.SetScreenSettingsOverride = (
  method: "emulation.setScreenSettingsOverride",
  params: emulation.SetScreenSettingsOverrideParameters
)

emulation.ScreenArea = {
  width: js-uint,
  height: js-uint
}

emulation.SetScreenSettingsOverrideParameters = {
  screenArea: emulation.ScreenArea / null,
  ? contexts: [+browsingContext.BrowsingContext],
  ? userContexts: [+browser.UserContext],
}
```

**Return Type:**
```cddl
emulation.SetScreenSettingsOverrideResult = EmptyResult
```

#### New Data Structures

**Screen Settings:**
- `height` (integer): Screen height
- `width` (integer): Screen width  
- `x` (integer): Screen x position
- `y` (integer): Screen y position

**Remote End Screen Settings Overrides:**
- `user context screen settings`: Weak map between user contexts and screen settings
- `navigable screen settings`: Weak map between navigables and screen settings

#### Behavior

The command sets emulated screen dimensions for:
- Specific browsing contexts (via `contexts` parameter)
- User contexts (via `userContexts` parameter)

Setting `screenArea` to `null` removes the override.

The command includes two WebDriver BiDi integration points:
- **emulated available screen area**: Returns override for navigable/user context or null
- **emulated total screen area**: Returns override for navigable/user context or null

---

### Section Renumbering

Due to the addition of `emulation.setScreenSettingsOverride`, subsequent emulation commands were renumbered:

| Command                                  | Old Section | New Section |
| ---------------------------------------- | ----------- | ----------- |
| `emulation.setScreenOrientationOverride` | 7.4.2.5     | 7.4.2.6     |
| `emulation.setUserAgentOverride`         | 7.4.2.6     | 7.4.2.7     |
| `emulation.setScriptingEnabled`          | 7.4.2.7     | 7.4.2.8     |
| `emulation.setTimezoneOverride`          | 7.4.2.8     | 7.4.2.9     |

---

### emulation.setNetworkConditions Result Type Fix

**Changed:** Return type documentation format standardized

**Before:**
```
Result Type
    EmptyResult
```

**After:**
```
Return Type

    emulation.SetNetworkConditionsResult = EmptyResult
```

---

## Index Updates

All references to renamed types and renumbered sections were updated throughout the specification index, including:
- CDDL key references
- CDDL value references  
- Definition references
- Method name references

---

## Implementation Notes

### High Priority
1. **Session Module**: Update type name from `session.SubscriptionRequest` to `session.SubscribeParameters` in subscribe command
2. **Emulation Module**: Implement `emulation.setScreenSettingsOverride` command with screen area override capability

### Low Priority
- Section number references updated (documentation only)
- Result type naming standardized for `setNetworkConditions`

### API Surface Changes
- **Added:** 1 new command (`emulation.setScreenSettingsOverride`)
- **Renamed:** 1 type (`session.SubscriptionRequest` → `session.SubscribeParameters`)
- **Added:** 2 new types (`emulation.ScreenArea`, `emulation.SetScreenSettingsOverrideParameters`)
- **Added:** 1 new result type (`emulation.SetScreenSettingsOverrideResult`)

---

## CDDL Index Changes Summary

**Added to EmulationCommand:**
```cddl
EmulationCommand = (
  ...
  emulation.SetScreenSettingsOverride /
  ...
)
```

**Added to EmulationResult:**
```cddl
EmulationResult = (
  ...
  emulation.SetScreenSettingsOverrideResult /
  ...
)
```

---

## References

- Previous spec: W3C Working Draft, 6 November 2025
- Current spec: W3C Working Draft, 20 November 2025
- Spec URL: https://www.w3.org/TR/2025/WD-webdriver-bidi-20251120
