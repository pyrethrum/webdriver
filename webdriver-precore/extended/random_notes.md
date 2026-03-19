

- locator ready to run
- css or xpath

===== Parent =====
- parent
- xpath
- xpath
- => use xpath

- xpath
- css | bidi only
- => double shot [BiDi start nodes | HTTP post filter - Find Element From Element]
- css child
- bidi child

==== Depth - inner text only ====
- bidi use direct
- HTTP Xpath - use xpath
- css - use js function

==== inner text ====
- bidi
- use direct
- HTTP Xpath - use xpath + conditional post attempt check for duplicate visible elements - consider js function
- css - use js function

==== Role ====
- BiDi - use native
- HTTP - use xpath approximation + post filter for duplicates - consider js function
- CSS use xpath + multi shot

- 


-  readers
-  gt fail info - failinfo
- postcheck when multiple locators detected
- hybrid locators
- wildcard text matching - js function
- could RunTimeFilter &&& RunTimeFilter
- reduce to filter
- run
- list of modifiers

-  readLoc @bool - isChecked Locator -> Reader Bool
-  readLoc
- Getter a / Setter a


HTTP 
GET 	/session/{session id}/element/{element id}/selected 	Is Element Selected
GET 	/session/{session id}/element/{element id}/attribute/{name} 	Get Element Attribute

GET 	/session/{session id}/element/{element id}/property/{name} 	Get Element Property
  - Returns a *JavaScript property* of the element (reflects live DOM state,
  - not the original HTML attribute). Reading `value` gives the current content
  - of an input; reading `checked` gives the current checked state of a checkbox.
  - Unlike getAttribute, this returns the typed JS value coerced to a string.
  - Example: GET .../property/value  on <input value="hello"> that the user has
  -          since cleared → returns "" (the live JS property), not "hello"
  -          (which getAttribute("value") would still return).
  -
  - Property names are JS DOM properties, *not* CSS properties (those belong to
  - /css/{property name}).  There is no single enumerated list — names come from
  - the IDL interfaces defined in two WHATWG Living Standards:
  -
  -   WebDriver endpoint spec:
  -     https://w3c.github.io/webdriver/#get-element-property
  -   HTML Living Standard — element IDL interfaces (primary source):
  -     https://html.spec.whatwg.org/multipage/indices.html#element-interfaces
  -   DOM Standard — base Element/Node properties:
  -     https://dom.spec.whatwg.org/#interface-element
  -
  - Common properties by category:
  -   All elements (DOM/HTML):
  -     id, className, tagName, innerHTML, outerHTML, innerText, textContent,
  -     hidden, tabIndex, title, lang, dir, accessKey
  -   Form controls (HTMLInputElement, HTMLTextAreaElement, HTMLSelectElement):
  -     value, checked, disabled, readOnly, required, multiple, type, name,
  -     placeholder, defaultValue, defaultChecked, selectedIndex,
  -     valueAsNumber, valueAsDate, min, max, step, pattern, maxLength, minLength
  -   Links / media (HTMLAnchorElement, HTMLImageElement, etc.):
  -     href, src, alt, width, height
  -   Select-specific:
  -     options, selectedOptions, size
  -
  - BiDi: NO direct protocol message. NodeProperties.attributes is *not* the
  - equivalent — it mirrors getAttribute() (original HTML attributes only).
  - To read a live JS property, use script.callFunction with the element's
  - sharedId as a SharedReference argument:
  -   script.callFunction {
  -     functionDeclaration: "(el, name) => el[name]",
  -     arguments: [{ sharedId: <id> }, { type: "string", value: "value" }]
  -   }

GET 	/session/{session id}/element/{element id}/css/{property name} 	Get Element CSS Value
  - Returns the *computed* CSS value for the given property name, as the browser
  - has resolved it (after applying the cascade, inheritance, and defaults).
  - The property name should be in camelCase or hyphenated form.
  - Colours are typically returned in rgba() notation; lengths in px.
  - Example: GET .../css/background-color  → "rgba(255, 99, 71, 1)"
  -          GET .../css/font-size         → "16px"
  -
  - BiDi: No direct message. Use script.callFunction:
  -   script.callFunction {
  -     functionDeclaration: "(el, p) => window.getComputedStyle(el).getPropertyValue(p)",
  -     arguments: [{ sharedId: <id> }, { type: "string", value: "background-color" }]
  -   }
  - Returns a script.StringRemoteValue.

GET 	/session/{session id}/element/{element id}/text 	Get Element Text
  - Returns the *visible rendered text* of the element and all its descendants,
  - whitespace-collapsed the same way a user sees it on screen (mirrors the
  - JS `element.innerText` semantics).  Hidden elements contribute no text.
  - Example: GET .../text  on <button>  Submit </button> → "Submit"
  -          GET .../text  on <p>Hello <span>world</span></p> → "Hello world"
  -
  - BiDi: NodeProperties.nodeValue only holds the raw value of a *text node*,
  - not the rendered text of an element. For innerText semantics use
  - script.callFunction:
  -   script.callFunction {
  -     functionDeclaration: "(el) => el.innerText",
  -     arguments: [{ sharedId: <id> }]
  -   }
  - Alternatively, serialize the node with NodeProperties.children and
  - recursively concatenate nodeValue of visible text-node descendants —
  - but this misses CSS-driven hiding, so script.callFunction is preferred.

GET 	/session/{session id}/element/{element id}/name 	Get Element Tag Name
  - Returns the *lowercase HTML tag name* of the element (equivalent to the JS
  - `element.tagName.toLowerCase()`).  Useful for asserting that you have
  - matched the right kind of element before interacting with it.
  - Example: GET .../name  on <INPUT type="text"> → "input"
  -          GET .../name  on <BUTTON>Click</BUTTON> → "button"
  -
  - BiDi: This is the *only* endpoint with a direct structural equivalent.
  - NodeProperties.localName in the script.NodeRemoteValue returned by
  - browsingContext.locateNodes already contains the lowercase tag name —
  - no extra round-trip needed.
  -   nodeRemoteValue.value.localName  ≡  "button"

GET 	/session/{session id}/element/{element id}/rect 	Get Element Rect
  - Returns a JSON object describing the element's *bounding rectangle* in the
  - viewport coordinate system: { x, y, width, height } (all in CSS pixels).
  - x/y are the top-left corner relative to the document origin.
  - Useful for checking visibility, position, or size without running JS.
  - Example: GET .../rect  → {"x": 120.5, "y": 340.0, "width": 200.0, "height": 48.0}
  -
  - BiDi: No direct message. Use script.callFunction:
  -   script.callFunction {
  -     functionDeclaration: "(el) => { const r = el.getBoundingClientRect();
  -                            return {x:r.x, y:r.y, width:r.width, height:r.height}; }",
  -     arguments: [{ sharedId: <id> }]
  -   }
  - Returns a script.ObjectRemoteValue with number-typed entries.

GET 	/session/{session id}/element/{element id}/enabled 	Is Element Enabled
  - Returns true/false indicating whether the element is currently *interactive*.
  - An element is disabled (false) when it carries the HTML `disabled` attribute
  - or is a form control inside a disabled <fieldset>.  Non-form elements (e.g.
  - a plain <div>) always return true regardless of any aria-disabled attribute.
  - Example: GET .../enabled  on <button disabled> → false
  -          GET .../enabled  on <input type="text"> → true
  -
  - BiDi: No direct message. Use script.callFunction:
  -   script.callFunction {
  -     functionDeclaration: "(el) => !el.disabled",
  -     arguments: [{ sharedId: <id> }]
  -   }
  - Note: NodeProperties.attributes may contain a "disabled" key, but only if
  - the attribute is present in the HTML source — it won't reflect a <fieldset>
  - disabled ancestor, making the JS property check more reliable.

GET 	/session/{session id}/element/{element id}/computedrole 	Get Computed Role
  - Returns the *accessibility role* the browser has resolved for the element,
  - taking both implicit (native HTML) roles and explicit `role=` attributes into
  - account.  This is what an assistive technology actually sees, so it is more
  - reliable for a11y assertions than checking the `role` attribute directly.
  - Example: GET .../computedrole  on <button> → "button"
  -          GET .../computedrole  on <div role="alert"> → "alert"
  -          GET .../computedrole  on <div> (no role) → "generic"
  -
  - BiDi: No direct message. Use script.callFunction via the Accessibility
  - Object Model (AOM) property el.computedRole (Chrome 90+, Firefox preview):
  -   script.callFunction {
  -     functionDeclaration: "(el) => el.computedRole",
  -     arguments: [{ sharedId: <id> }]
  -   }
  - NodeProperties.attributes["role"] only gives the *explicit* attribute value,
  - missing all implicit roles from native HTML semantics.

GET 	/session/{session id}/element/{element id}/computedlabel 	Get Computed Label
  - Returns the *accessible name* of the element as computed by the browser's
  - accessible name and description computation algorithm (ARIA spec).  This
  - aggregates aria-label, aria-labelledby, the element's own text content,
  - associated <label> elements, alt text, title, placeholder, etc. — the same
  - string a screen reader would announce.
  - Example: GET .../computedlabel  on <button aria-label="Close dialog"> → "Close dialog"
  -          GET .../computedlabel  on <input id="x"><label for="x">Email</label> → "Email"
  -
  - BiDi: No direct message. Use script.callFunction via the AOM property
  - el.computedLabel (Chrome 90+):
  -   script.callFunction {
  -     functionDeclaration: "(el) => el.computedLabel",
  -     arguments: [{ sharedId: <id> }]
  -   }
  - NodeProperties.attributes can surface "aria-label" directly, but that is
  - only one of many labelling sources — it misses aria-labelledby (requires
  - following an idref), inner text, <label for=...>, alt, title, etc.
  - el.computedLabel is the only way to get the fully resolved accessible name
  - without reimplementing the entire ARIA naming algorithm in the client.

BiDi ::
browsingContext.LocateNodes = (
  method: "browsingContext.locateNodes",
  params: browsingContext.LocateNodesParameters
)

browsingContext.LocateNodesParameters = {
   context: browsingContext.BrowsingContext,
   locator: browsingContext.Locator,
   ? maxNodeCount: (js-uint .ge 1),
   ? serializationOptions: script.SerializationOptions,
   ? startNodes: [ + script.SharedReference ]
}

=>

browsingContext.LocateNodesResult = {
    nodes: [ * script.NodeRemoteValue ]
}

script.NodeRemoteValue = {
  type: "node",
  ? sharedId: script.SharedId,
  ? handle: script.Handle,
  ? internalId: script.InternalId,
  ? value: script.NodeProperties,
}

script.NodeProperties = {
  nodeType: js-uint,
  childNodeCount: js-uint,
  ? attributes: {*text => text},
  ? children: [*script.NodeRemoteValue],
  ? localName: text,
  ? mode: "open" / "closed",
  ? namespaceURI: text,
  ? nodeValue: text,
  ? shadowRoot: script.NodeRemoteValue / null,
}

script.WindowProxyRemoteValue = {
  type: "window",
  value: script.WindowProxyProperties,
  ? handle: script.Handle,
  ? internalId: script.InternalId
}

-}

{- https://chatgpt.com/c/69ab3adc-94bc-8324-bf32-218569fcaf86
function bidiIsVisible(el) {
  if (!el || !el.isConnected) return false;

  const style = getComputedStyle(el);

  if (style.display === "none") return false;
  if (style.visibility === "hidden" || style.visibility === "collapse") return false;mplification,
          test_flatenning_no

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

Performance tip (important)

For automation frameworks, inject the helper once per page:

window.__bidiIsVisible = bidiIsVisible

Then call:

__bidiIsVisible(element)

This avoids re-sending the function each time

-}
- Looks like we need a GADT

{- riffing
  ### locator

- `Form` type - polymorphic on selector or sting bool so record types are paired (possible use for associated types)
	- not sure how this would work (how to do reflection)
	- `setForm`
	- could setform even be an effect?
	- how would this differ from palin old do notation

- `ensure` selectors - wait till condition is satisfied
- or make locators a predicate at heart with monoid and alternative instances so you could have `byId` or `withClass` or `containsText` or `withText` or `withText'`  `withTextFilter`  `elmFilter` ...

  use default string rules and a typeclass and a newtype around text to get default string treatment
- l

  ```haskell
  IsLocator

  - locators lieient by default ??

  Locator == id
  LocatorText
  ```

  ```haskell
   click $
     byCSS ".highlight"
      <> placeholder "enter here"
     <|> button "Submit"

```haskell
 click $
   byCSS ".highlight"
    && placeholder "enter here"
    || button "Submit"
```

    submitButton = button <> hasText "Submit"
    buttonTxt = (<>) button . hasText

    submitButton = buttonTxt "Submit"

    clickById
    clickByRoll
    fillByCSS

    or
     byId s = byCSS $ "#" <> s
     button = (<>) button_ . hasText
     submitButton = button "Submit"

  - default string treatment
  click submit_button
  set "given_name" "John"

  - globs by default

  welcomeMessage = hasText "Hello * welcome to Myer"

  hasText' [NoGlobs, CaseSensitive, FullMatch] "Hello * welcome to Myer"

  hasParent Locator
  hasDirectParent Locator

  - an edit type which is an id and a value and has a monoid instance

  Edit type

  set EDIT
  set $
   "given_name" ~> "John"
   <> "last_name" "Walker"
   <> byPlaceholder "Title" "Sir"

- I doubt this would work ??
- use GADTs ? or just a parameterised data type wiith one param type for Locators and another for values

perhaps a data type with selector -> a -> m () that could be partially applied

--
Yes, you can select elements based on their `value` in Playwright, though the approach differs slightly between standard HTML elements and custom components.

### 🎯 For Standard HTML Elements

The most direct way is to use standard CSS attribute selectors. This is particularly effective for `<input>`, `<button>`, and `<option>` elements where the `value` is set as an HTML attribute .

```javascript
// Select an input button by its value
await page.locator('input[value="Log in"]').click();

// Select any element with a specific value attribute
const element = page.locator('[value="your-value-here"]');
```

For `<select>` dropdowns, Playwright provides the dedicated `selectOption()` method, which can select by `value`, visible `label`, or `index` .

```javascript
// Select by the option's value attribute
await page.locator('select#country').selectOption('us');

// Select by the option's visible text
await page.locator('select#country').selectOption({ label: 'United States' });
```

### ⚠️ For Custom Components

Modern frontend frameworks often build custom dropdowns and inputs that don't use standard HTML `<select>` or simple `<input>` elements. In these cases, the `value` you see may be a property of the JavaScript object, not an HTML attribute, so CSS selectors won't work directly .

You have a couple of options to handle this:

1.  **Interact by visible text**: Click the custom component to open it, then select the option by its visible text.
    ```javascript
    await page.getByTestId('custom-dropdown-trigger').click();
    await page.getByText('Desired Option', { exact: true }).click();
    ```
2.  **Use a more resilient locator strategy**: Rely on `getByTestId()`, `getByRole()`, or other stable attributes provided by the component, rather than the element's state .

### 🔍 Checking Values After Selection

Once you've interacted with an element, you'll often want to verify it has the correct value. Playwright provides specific methods for this :

- **`toHaveValue()`**: An assertion to verify the value of an `<input>`, `<textarea>`, or `<select>` element.
    ```javascript
    await expect(page.locator('input#name')).toHaveValue('John Doe');
    ```
- **`inputValue()`**: To retrieve the current value of an input element without making an assertion.
    ```javascript
    const currentValue = await page.locator('input#name').inputValue();
    ```

I hope this helps you effectively select and interact with elements based on their values. If you're dealing with a particularly tricky component, feel free to share the HTML structure, and I might be able to offer a more specific suggestion
