- [BiDi Accessibility Locators vs CSS](#bidi-accessibility-locators-vs-css)
  - [`role` → CSS `[role=...]`](#role--css-role)
  - [`name` (accessible name) → no direct CSS equivalent](#name-accessible-name--no-direct-css-equivalent)
  - [Practical Upshot](#practical-upshot)
- [Role Approx](#role-approx)
  - [Edge Cases Not Covered](#edge-cases-not-covered)
    - [1. **aria-labelledby References**](#1-aria-labelledby-references)
    - [2. **`<label for="...">` Associations**](#2-label-for-associations)
    - [3. **Shadow DOM**](#3-shadow-dom)
    - [4. ~~**NA Description Unless extend high level locaotrs - aria-describedby (Description, Not Name)**~~](#4-na-description-unless-extend-high-level-locaotrs---aria-describedby-description-not-name)
    - [5. **Accessible Name Computation Order**](#5-accessible-name-computation-order)
    - [6. **Hidden Text in Accessible Names**](#6-hidden-text-in-accessible-names)
    - [7. **Recursive/Nested aria-labelledby**~~](#7-recursivenested-aria-labelledby)
    - [~~9. **Role Presentation/None Suppression**~~](#9-role-presentationnone-suppression)
    - [10. **Form Control Implicit Labels**](#10-form-control-implicit-labels)
    - [11. **ARIA 1.3 Name-from-Content Rules**](#11-aria-13-name-from-content-rules)
    - [12. **Internationalization/Whitespace**](#12-internationalizationwhitespace)
    - [Recommendation](#recommendation)
- [Visible Text](#visible-text)
- [SubContext - IFrames](#subcontext---iframes)
  - [BiDi vs HTTP WebDriver: Interacting with Elements in Child iframes](#bidi-vs-http-webdriver-interacting-with-elements-in-child-iframes)
    - [BiDi — no switching, context IDs are stable handles](#bidi--no-switching-context-ids-are-stable-handles)
    - [HTTP WebDriver — stateful switch required](#http-webdriver--stateful-switch-required)
    - [Key Difference](#key-difference)
- [Displayed](#displayed)
  - [Hybrid “best practical” visibility function](#hybrid-best-practical-visibility-function)
  - [Why this works very well](#why-this-works-very-well)
    - [1. CSS visibility rules](#1-css-visibility-rules)
    - [2. Layout presence](#2-layout-presence)
    - [3. Real user visibility](#3-real-user-visibility)
  - [Why multiple hit points matter](#why-multiple-hit-points-matter)
  - [Example BiDi call](#example-bidi-call)
  - [Performance tip (important)](#performance-tip-important)
  - [Behaviour vs WebDriver `displayed`](#behaviour-vs-webdriver-displayed)
- [Depth](#depth)
  - [claude notes on visibility performance but need to check](#claude-notes-on-visibility-performance-but-need-to-check)
  - [BiDi (`script.evaluate` over WebSocket)](#bidi-scriptevaluate-over-websocket)
  - [Classic HTTP WebDriver (`executeScript`)](#classic-http-webdriver-executescript)
  - [Summary](#summary)
- [Getting Value](#getting-value)
  - [Classic WebDriver (HTTP)](#classic-webdriver-http)
  - [WebDriver BiDi](#webdriver-bidi)





# BiDi Accessibility Locators vs CSS

Partial and imperfect. The rough mapping:

## `role` → CSS `[role=...]`

Works only for **explicit** ARIA role attributes:

```css
[role="button"]   /* works for explicit role= attributes */
```

But native semantic elements (`<button>`, `<h1>`, `<nav>`) have *implicit* roles that CSS can't query by role — you'd need to know the element type. No CSS equivalent for the implicit role of `<button>` other than `button`.

## `name` (accessible name) → no direct CSS equivalent

The accessible name is a *computed* property derived from (in priority order):

1. `aria-labelledby` → text content of another element
2. `aria-label` → attribute value
3. `<label>` association
4. `placeholder`, `title`, `alt` depending on element type
5. Inner text content

You can approximate some cases:

```css
[aria-label="First name"]       /* only covers aria-label case */
[placeholder="First name"]      /* only covers placeholder case */
```

But there's no single CSS selector that captures all sources of accessible name.

## Practical Upshot

Accessibility locators are semantically richer — they work against the *accessibility tree* (what screen readers see), not the DOM. The BiDi spec intentionally delegates evaluation to the browser's AX tree computation rather than DOM attributes, so they're not reliably reducible to CSS.

That's most of their value — a `role=button` locator matches `<button>`, `<div role="button">`, `<input type="button">` etc. uniformly.


# Role Approx


-- | Best-effort XPath approximation of a BiDi accessibility locator.
--   Covers explicit @role attributes, implicit roles for common HTML elements,
--   and common accessible name sources. Cannot cover aria-labelledby,
--   label[for=...], or shadow DOM.
-- TODO: cover edge cases in execution


User: -- | Best-effort XPath approximation of a BiDi accessibility locator.
--   Covers explicit @role attributes, implicit roles for common HTML elements,
--   and common accessible name sources. Cannot cover aria-labelledby,
--   label[for=...], or shadow DOM.

expand on edge cases missede by this function

GitHub Copilot: Based on the function `accessibilityToXPath` and the ARIA/HTML specifications, here are the edge cases this XPath approximation misses:

## Edge Cases Not Covered

### 1. **aria-labelledby References**
The function cannot resolve `aria-labelledby` which references other element IDs to construct the accessible name. XPath 1.0 has no way to follow ID references and concatenate text from multiple elements.

`aria-labelledby` also accepts a **space-separated list of IDs** — the browser concatenates the text of all referenced elements in order to form the accessible name. XPath cannot resolve even a single idref, let alone multiple.

```html
<!-- Single idref: -->
<h2 id="dialog-title">Confirm Action</h2>
<button aria-labelledby="dialog-title">OK</button>
<!-- Accessible name: "Confirm Action" -->

<!-- Multiple idrefs (valid and common in dialogs): -->
<h2 id="dialog-title">Delete File</h2>
<p  id="dialog-subtitle">This action cannot be undone.</p>
<button aria-labelledby="dialog-title dialog-subtitle">Confirm</button>
<!-- Accessible name: "Delete File This action cannot be undone." -->
<!-- XPath cannot resolve either reference, let alone concatenate both -->
```

### 2. **`<label for="...">` Associations**
HTML `<label>` elements that reference form controls via the `for` attribute create accessible names, but XPath cannot make this reverse lookup.

```html
<!-- Cannot handle this: -->
<label for="email-input">Email Address</label>
<input id="email-input" type="email">
<!-- Accessible name is "Email Address", but XPath can't follow the for/id link -->
```

### 3. **Shadow DOM**
Elements inside shadow roots are completely invisible to XPath queries from the light DOM.

```html
<!-- Cannot handle this: -->
<custom-button>
  #shadow-root
    <button role="button">Click Me</button>
</custom-button>
```

### 4. ~~**NA Description Unless extend high level locaotrs - aria-describedby (Description, Not Name)**~~
~~While the function focuses on accessible *names*, `aria-describedby` contributes to the accessible *description* and is similarly unsupported.~~

~~Like `aria-labelledby`, `aria-describedby` is also an **ID reference list** — it accepts multiple space-separated IDs, and the browser concatenates the text of all referenced elements to form the description. The same applies to `aria-controls`, `aria-owns`, and `aria-flowto`. None of these can be resolved by XPath.~~

```html
<!-- Multiple idrefs on aria-describedby: -->
<p id="hint-1">Must be at least 8 characters.</p>
<p id="hint-2">Must include a number.</p>
<input type="password" aria-label="Password" aria-describedby="hint-1 hint-2">
<!-- Accessible description: "Must be at least 8 characters. Must include a number." -->
```

### 5. **Accessible Name Computation Order**
The [ARIA spec's name computation algorithm](https://www.w3.org/TR/accname-1.2/#mapping_additional_nd_te) has a specific precedence order. This function checks all sources with `or`, but the real algorithm stops at the first match:

1. `aria-labelledby` (highest priority)
2. `aria-label`
3. Native HTML labeling (`<label for>`, `alt`, `title`, etc.)
4. Inner text content (lowest priority)

The XPath treats them as equals, potentially matching elements that wouldn't match in real accessibility APIs.

### 6. **Hidden Text in Accessible Names**
CSS-hidden text (`visibility: hidden`, `display: none`) can still contribute to accessible names via `aria-label` or `aria-labelledby`, but those elements wouldn't match the XPath visibility filters you use elsewhere.

### 7. **Recursive/Nested aria-labelledby**~~
Elements referenced by `aria-labelledby` can themselves have `aria-labelledby`, creating chains. Real browsers flatten these recursively; XPath cannot.

~~### 8. **Title Attribute Fallback Conditions**~~
~~The `title` attribute only becomes the accessible name when no other labeling mechanism exists. The XPath uses `or`, so it might match elements where `title` shouldn't be considered.~~

### ~~9. **Role Presentation/None Suppression**~~
~~Elements with `role="presentation"` or `role="none"` have their semantics (and often their names) suppressed in the accessibility tree, but this function still tries to match them.~~

```html
<!-- Should not expose accessible name: -->
<img src="decorative.png" alt="Decorative" role="none">
```

### 10. **Form Control Implicit Labels**
Wrapping a form control inside a `<label>` without using `for=` creates an implicit association:

```html
<!-- Cannot handle this: -->
<label>
  Username
  <input type="text">
</label>
```

### 11. **ARIA 1.3 Name-from-Content Rules**
Some roles prohibit name-from-content (e.g., `textbox`, `combobox`). Others require it (e.g., `button`, `link`). The function doesn't enforce these constraints.

### 12. **Internationalization/Whitespace**
`normalize-space()` handles basic whitespace, but doesn't account for RTL text, zero-width characters, or Unicode normalization differences that might affect accessible name matching.

### Recommendation

For production use, consider documenting these limitations prominently and/or falling back to BiDi's native `browsingContext.Locator` with `type: "accessibility"` when the session supports it,
using this XPath only as a best-effort HTTP fallback.

# Visible Text

 *Best-effort XPath approximation of a BiDi inner text locator.
 Uses normalize-space(.) as a proxy for innerText. Case-insensitive
 matching uses the translate() alphabet hack. maxDepth is approximated
 via count(ancestor::*).
 Visibility filtering: excludes elements with the HTML @hidden attribute,
 @aria-hidden='true', or inline style display:none / visibility:hidden.
ODO: check strategy for visibility filtering in execution - may need to add more cases or use script.callFunction instead
  Does NOT catch hiding via CSS classes or inherited/cascaded styles —
  only the BiDi innerText locator handles those correctly.*

# SubContext - IFrames

## BiDi vs HTTP WebDriver: Interacting with Elements in Child iframes

**Scenario:** On main page with a child `<iframe>`, edit an element with id `firstName` inside the child frame.


### BiDi — no switching, context IDs are stable handles

```
# 1. Discover child context
browsingContext.getTree({ root: main_ctx })
→ children: [{ context: child_ctx, ... }]

# 2. Locate node directly in child context
browsingContext.locateNodes({
  context: child_ctx,
  locator: { type: "css", value: "#firstName" }
})
→ nodes: [{ sharedId: "node-abc" }]

# 3. Interact
perform_actions*(child_ctx, click node-abc)
perform_actions*(child_ctx, type "John")
```

### HTTP WebDriver — stateful switch required

```
# 1. Find the iframe element in parent
POST /session/{id}/element  { using: "css selector", value: "iframe" }
→ elementId: "elem-xyz"

# 2. Switch into frame (global state change)
POST /session/{id}/frame  { id: { element-6066...: "elem-xyz" } }

# 3. Now find #firstName (relative to frame)
POST /session/{id}/element  { using: "css selector", value: "#firstName" }
→ elementId: "elem-abc"

# 4. Interact
perform_actions*(click elem-abc, type "John")

# 5. Must switch back
POST /session/{id}/frame  { id: null }
```

### Key Difference

BiDi contexts are **first-class persistent handles** so you just address the child context directly. HTTP WebDriver has a single implicit "current frame" cursor that you have to move around and restore — easy to leak state if something throws between switch-in and switch-back.

# Displayed

Below is a **robust hybrid visibility function (~40 lines)** that combines ideas from:

* **Selenium / W3C WebDriver** (`isDisplayed` style checks)
* **Playwright** (hit-testing using `elementFromPoint`)
* modern browser layout behavior

It works very well when called through **WebDriver BiDi** `script.evaluate`.

This version checks:

* DOM attachment
* `display` / `visibility`
* hidden input
* layout box existence
* viewport intersection
* occlusion (hit-testing)
* partial overlays (multiple hit points)

---

## Hybrid “best practical” visibility function

```javascript
function bidiIsVisible(el) {
  if (!el || !el.isConnected) return false;

  const style = getComputedStyle(el);

  if (style.display === "none") return false;
  if (style.visibility === "hidden" || style.visibility === "collapse") return false;

  if (el.tagName === "INPUT" && el.type === "hidden")
    return false;

  const rect = el.getBoundingClientRect();

  if (rect.width === 0 || rect.height === 0)
    return false;

  const vpW = window.innerWidth;
  const vpH = window.innerHeight;

  if (
    rect.bottom < 0 ||
    rect.right < 0 ||
    rect.top > vpH ||
    rect.left > vpW
  )
    return false;

  const points = [
    [rect.left + rect.width / 2, rect.top + rect.height / 2],
    [rect.left + 1, rect.top + 1],
    [rect.right - 1, rect.bottom - 1]
  ];

  for (const [x, y] of points) {
    if (x < 0 || y < 0 || x > vpW || y > vpH)
      continue;

    const hit = document.elementFromPoint(x, y);

    if (hit === el || el.contains(hit))
      return true;
  }

  return false;
}
```

---

## Why this works very well

### 1. CSS visibility rules

Handled via:

```
display
visibility
hidden input
```

### 2. Layout presence

Handled via:

```
getBoundingClientRect()
```

### 3. Real user visibility

Handled via:

```
document.elementFromPoint()
```

Which automatically respects:

* `z-index`
* overlays
* transforms
* clipping
* stacking contexts
* scroll containers
* `pointer-events`

---

## Why multiple hit points matter

Example:

```
+--------------+
|  overlay     |
|      X       |
|              |
| element area |
+--------------+
```

If the **center** is covered but a corner is visible, the element should still count as visible.

Checking **3 points** dramatically reduces false negatives.

---

## Example BiDi call

```json
{
  "method": "script.evaluate",
  "params": {
    "expression": "bidiIsVisible(arguments[0])",
    "target": { "context": "context-id" },
    "awaitPromise": false,
    "arguments": [
      { "handle": "element-handle-id" }
    ]
  }
}
```

---

## Performance tip (important)

For automation frameworks, inject the helper **once per page**:

```js
window.__bidiIsVisible = bidiIsVisible
```

Then call:

```
__bidiIsVisible(element)
```

This avoids re-sending the function each time.

---

## Behaviour vs WebDriver `displayed`

| Scenario              | WebDriver         | Hybrid   |
| --------------------- | ----------------- | -------- |
| `display:none`        | hidden            | hidden   |
| `visibility:hidden`   | hidden            | hidden   |
| zero size             | hidden            | hidden   |
| offscreen             | sometimes visible | hidden   |
| covered by overlay    | visible ❌         | hidden ✅ |
| CSS transform clipped | unreliable        | correct  |

---

✅ **Bottom line**

For **BiDi-based automation**, this hybrid algorithm is usually **more accurate than the classic WebDriver `displayed` command** while being much simpler.

---

If you'd like, I can also show a **very useful BiDi technique used by browser automation frameworks** that lets you implement **`waitUntilDisplayed` without polling from the client** (much faster and cleaner).


# Depth

- xpath
- double shot
  - bidi params
  - elem from

---

## claude notes on visibility performance but need to check

Conclusion - for now untill tested - always inject
Based on the protocol mechanics and known JS engine behavior, here are approximate magnitudes:

## BiDi (`script.evaluate` over WebSocket)

The `bidiIsVisible` function is ~600 bytes of JS source.

| Cost                       | Per-call injection  | Pre-injected         |
| -------------------------- | ------------------- | -------------------- |
| WebSocket frame payload    | ~650 bytes          | ~35 bytes            |
| JSON serialization         | ~0.05–0.2ms         | negligible           |
| JS parse + compile (V8/SM) | ~0.05–0.3ms         | 0 (already compiled) |
| **Total extra overhead**   | **~0.1–0.5ms/call** | baseline             |

Over 100 visibility checks: roughly **10–50ms** extra for BiDi.

## Classic HTTP WebDriver (`executeScript`)

HTTP already has much higher baseline latency (TCP + HTTP framing, even on localhost ~1–5ms), so the extra payload matters less proportionally:

| Cost                     | Per-call injection  | Pre-injected |
| ------------------------ | ------------------- | ------------ |
| HTTP body size           | ~700 bytes          | ~80 bytes    |
| Serialization overhead   | ~0.1–0.5ms          | negligible   |
| JS parse + compile       | ~0.05–0.3ms         | 0            |
| **Total extra overhead** | **~0.1–0.8ms/call** | baseline     |

The HTTP round-trip (1–5ms localhost, 10–100ms remote) **dominates**, so the function injection overhead is a **smaller relative fraction** for HTTP than for BiDi.

## Summary

- **BiDi**: ~0.1–0.5ms/call overhead — matters noticeably at scale (50+ checks/page)
- **HTTP classic**: ~0.1–0.8ms/call overhead — relatively minor against the HTTP RTT, but still accumulates
- **Crossover point**: At 20–50 element checks per page, pre-injection starts paying off meaningfully for BiDi; for HTTP it's primarily worthwhile for very high check counts or remote drivers

The JS engine parse cost is the dominant factor for BiDi (where the WebSocket RTT is tiny), while for HTTP classic it's largely noise against network latency.

# Getting Value

Searched for regex `getProperty|getAttribute|element.*value|script.evaluate|callFunction` (`**/*.hs`), 20 results

Searched for files matching `**/webdriver-precore/src/WebDriverPreCore`, no matches

Searched for text `getElementProperty` (`**/webdriver-precore/src/WebDriverPreCore/HTTP/SpecDefinition.hs`), 3 results

Searched for text `script` (`**/webdriver-precore/src/WebDriverPreCore/Bidi`), no results

Searched for files matching `**/Bidi/**/*.hs`, 20 matches

Read [](file:///home/john-walker/repos/webdriver/webdriver-precore/src/WebDriverPreCore/HTTP/SpecDefinition.hs#725-725), lines 725 to 745

Searched for text `callFunction|evaluate|RemoteValue|node` (`**/webdriver-precore/src/WebDriverPreCore/BiDi/Script.hs`), 20 results

Searched for text `CallFunction` (`**/webdriver-precore/src/WebDriverPreCore/BiDi/Script.hs`), 6 results

Read [](file:///home/john-walker/repos/webdriver/webdriver-precore/src/WebDriverPreCore/BiDi/Script.hs#285-285), lines 285 to 340

Here's the answer:

## Classic WebDriver (HTTP)

There are two relevant endpoints depending on what "value" means:

**DOM Property (current live value — what you usually want for inputs):**
```
GET /session/{sessionId}/element/{elementId}/property/value
```
Returns the current DOM `.value` property. This reflects what the user has typed.

**HTML Attribute (static initial value from markup):**
```
GET /session/{sessionId}/element/{elementId}/attribute/value
```
Returns the `value` HTML attribute — the default/reset value, not necessarily the current one.

For most use cases (e.g. checking what's in an `<input>`), you want the **property** endpoint.

---

## WebDriver BiDi

BiDi has no dedicated "get element property" command. Instead you use **`script.callFunction`**, passing the element as a `SharedReference` (via its `sharedId`):

```json
{
  "method": "script.callFunction",
  "params": {
    "functionDeclaration": "function(el) { return el.value; }",
    "awaitPromise": false,
    "target": { "context": "<browsingContextId>" },
    "arguments": [
      { "sharedId": "<elementSharedId>" }
    ]
  }
}
```

The response comes back as a `script.EvaluateResult` with the value as a `RemoteValue` (a `StringValue` for a text input).

Alternatively, `script.evaluate` works for simple cases if the element reference is already in scope, but `script.callFunction` with a `SharedReference` argument is the standard pattern when you already have a located element.


