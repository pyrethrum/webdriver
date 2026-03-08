module WebDriverPreCore.Extended.Locators where

import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Text (Text, intercalate, pack, toLower)
import WebDriverPreCore.Extended.BiDi.Base.Protocol (BrowsingContext, JSUInt)
import WebDriverPreCore.Extended.Tags (Tag)
import Prelude

-- TODO : use bidi type when merged

css :: Text -> Locator
css = CSS

--
-- data LocatorType = General | HttP | BiDi --- ????s

-- | Locator for use with both HTTP and BiDi protocols.
data Locator
  = Role {role :: Maybe AriaRole, name :: Maybe Text}
  | CSS {value :: Text}
  | -- browsingContextId -> elementId ie get the frame that belongs to the browsing context
    BiDiContext {context :: BrowsingContext}
  | InnerText
      { value :: Text,
        ignoreCase :: Maybe Bool,
        matchType :: Maybe MatchType,
        maxDepth :: Maybe JSUInt
      }
  | XPath {value :: Text}
  | Parent {parent :: Locator, child :: Locator}
  | And (NonEmpty Locator)
  | Or (NonEmpty Locator)
  | Not (NonEmpty Locator)
  | Default Text
  | BiDiDirective
      { description :: Text,
        -- TODO: fix this when merged
        biDiCommand :: Either Text Text
      }
  | HttpDirective
      { description :: Text,
        -- TODO: fix this when merged
        httpCommand :: Either Text Text
      }
  | JSDirective
      { description :: Text,
        -- TODO: fix this when merged
        js :: Text
      }
  deriving (Show, Eq)

------- Role Smart Constructors -------

role' :: Maybe AriaRole -> Maybe Text -> Locator
role' r = Role r

role :: AriaRole -> Text -> Locator
role r = role' (Just r) . Just

article :: Text -> Locator
article = role Article

banner :: Text -> Locator
banner = role Banner

button :: Text -> Locator
button = role Button

cell :: Text -> Locator
cell = role Cell

checkbox :: Text -> Locator
checkbox = role Checkbox

columnHeader :: Text -> Locator
columnHeader = role ColumnHeader

complementary :: Text -> Locator
complementary = role Complementary

contentInfo :: Text -> Locator
contentInfo = role ContentInfo

definition :: Text -> Locator
definition = role Definition

dialog :: Text -> Locator
dialog = role Dialog

figure :: Text -> Locator
figure = role Figure

form :: Text -> Locator
form = role Form

group :: Text -> Locator
group = role Group

heading :: Text -> Locator
heading = role Heading

img :: Text -> Locator
img = role Img

link :: Text -> Locator
link = role Link

list :: Text -> Locator
list = role List

listItem :: Text -> Locator
listItem = role ListItem

mainRole :: Text -> Locator
mainRole = role Main

navigation :: Text -> Locator
navigation = role Navigation

option :: Text -> Locator
option = role Option

progressBar :: Text -> Locator
progressBar = role ProgressBar

radio :: Text -> Locator
radio = role Radio

region :: Text -> Locator
region = role Region

row :: Text -> Locator
row = role Row

rowHeader :: Text -> Locator
rowHeader = role RowHeader

search :: Text -> Locator
search = role Search

separator :: Text -> Locator
separator = role Separator

slider :: Text -> Locator
slider = role Slider

spinButton :: Text -> Locator
spinButton = role SpinButton

status :: Text -> Locator
status = role Status

table :: Text -> Locator
table = role Table

term :: Text -> Locator
term = role Term

textbox :: Text -> Locator
textbox = role Textbox

(&&&) :: Locator -> Locator -> Locator
l &&& r = And (l :| [r])

infixr 3 &&&

(|||) :: Locator -> Locator -> Locator
l ||| r = Or (l :| [r])

infixr 2 |||

notLoc :: Locator -> Locator
notLoc l = Not (l :| [])

-- | Recursively flattens and simplifies Match* locators while maintaining logical correctness.
-- Flattens nested Match* of the same type and applies De Morgan's laws where applicable.
flattenLoc :: Locator -> Locator
flattenLoc = \case
  -- Flatten And: And [And [a,b], c] -> And [a,b,c]
  And locs ->
    let reduced = flattenLoc <$> locs
        flattened = concatMap flattenAll reduced
     in case flattened of
          [single] -> single
          (x : xs) -> And (x :| xs)
          [] -> error "flattenLoc: And produced empty list (impossible with NonEmpty input)"
    where
      flattenAll (And xs) = toList xs
      flattenAll x = [x]

  -- Flatten Or: Or [Or [a,b], c] -> Or [a,b,c]
  Or locs ->
    let reduced = flattenLoc <$> locs
        flattened = concatMap flattenAny reduced
     in case flattened of
          [single] -> single
          (x : xs) -> Or (x :| xs)
          [] -> error "flattenLoc: Or produced empty list (impossible with NonEmpty input)"
    where
      flattenAny (Or xs) = toList xs
      flattenAny x = [x]

  -- Apply De Morgan's laws and flatten Not
  Not locs ->
    let reduced = flattenLoc <$> locs
     in case toList reduced of
          -- Double negation: Not [Not [x]] -> Or [x]
          [Not xs] -> flattenLoc $ Or xs
          -- De Morgan: Not [And [x,y]] -> Or [Not [x], Not [y]]
          [And xs] -> flattenLoc . Or $ (\x -> Not (x :| [])) <$> xs
          -- De Morgan: Not [Or [x,y]] -> And [Not [x], Not [y]]
          [Or xs] -> flattenLoc . And $ (\x -> Not (x :| [])) <$> xs
          -- Single non-Match* locator - already reduced
          [single] -> Not (single :| [])
          -- Multiple locators - can't simplify further
          (x : xs) -> Not (x :| xs)
          [] -> error "flattenLoc: Not produced empty list (impossible with NonEmpty input)"
  -- Parent with recursive reduction on both sides
  Parent p c -> Parent (flattenLoc p) $ flattenLoc c
  -- All other locator types remain unchanged
  loc -> loc

--  readers
--  gt fail info - failinfo
-- postcheck when multiple locators detected
-- hybrid locators
-- wildcard text matching - js function
-- could RunTimeFilter &&& RunTimeFilter
-- reduce to filter
-- run
-- list of modifiers

--  readLoc @bool - isChecked Locator -> Reader Bool
--  readLoc
-- Getter a / Setter a

{-
HTTP ::
GET 	/session/{session id}/element/{element id}/selected 	Is Element Selected
GET 	/session/{session id}/element/{element id}/attribute/{name} 	Get Element Attribute

GET 	/session/{session id}/element/{element id}/property/{name} 	Get Element Property
  -- Returns a *JavaScript property* of the element (reflects live DOM state,
  -- not the original HTML attribute). Reading `value` gives the current content
  -- of an input; reading `checked` gives the current checked state of a checkbox.
  -- Unlike getAttribute, this returns the typed JS value coerced to a string.
  -- Example: GET .../property/value  on <input value="hello"> that the user has
  --          since cleared → returns "" (the live JS property), not "hello"
  --          (which getAttribute("value") would still return).
  --
  -- Property names are JS DOM properties, *not* CSS properties (those belong to
  -- /css/{property name}).  There is no single enumerated list — names come from
  -- the IDL interfaces defined in two WHATWG Living Standards:
  --
  --   WebDriver endpoint spec:
  --     https://w3c.github.io/webdriver/#get-element-property
  --   HTML Living Standard — element IDL interfaces (primary source):
  --     https://html.spec.whatwg.org/multipage/indices.html#element-interfaces
  --   DOM Standard — base Element/Node properties:
  --     https://dom.spec.whatwg.org/#interface-element
  --
  -- Common properties by category:
  --   All elements (DOM/HTML):
  --     id, className, tagName, innerHTML, outerHTML, innerText, textContent,
  --     hidden, tabIndex, title, lang, dir, accessKey
  --   Form controls (HTMLInputElement, HTMLTextAreaElement, HTMLSelectElement):
  --     value, checked, disabled, readOnly, required, multiple, type, name,
  --     placeholder, defaultValue, defaultChecked, selectedIndex,
  --     valueAsNumber, valueAsDate, min, max, step, pattern, maxLength, minLength
  --   Links / media (HTMLAnchorElement, HTMLImageElement, etc.):
  --     href, src, alt, width, height
  --   Select-specific:
  --     options, selectedOptions, size
  --
  -- BiDi: NO direct protocol message. NodeProperties.attributes is *not* the
  -- equivalent — it mirrors getAttribute() (original HTML attributes only).
  -- To read a live JS property, use script.callFunction with the element's
  -- sharedId as a SharedReference argument:
  --   script.callFunction {
  --     functionDeclaration: "(el, name) => el[name]",
  --     arguments: [{ sharedId: <id> }, { type: "string", value: "value" }]
  --   }

GET 	/session/{session id}/element/{element id}/css/{property name} 	Get Element CSS Value
  -- Returns the *computed* CSS value for the given property name, as the browser
  -- has resolved it (after applying the cascade, inheritance, and defaults).
  -- The property name should be in camelCase or hyphenated form.
  -- Colours are typically returned in rgba() notation; lengths in px.
  -- Example: GET .../css/background-color  → "rgba(255, 99, 71, 1)"
  --          GET .../css/font-size         → "16px"
  --
  -- BiDi: No direct message. Use script.callFunction:
  --   script.callFunction {
  --     functionDeclaration: "(el, p) => window.getComputedStyle(el).getPropertyValue(p)",
  --     arguments: [{ sharedId: <id> }, { type: "string", value: "background-color" }]
  --   }
  -- Returns a script.StringRemoteValue.

GET 	/session/{session id}/element/{element id}/text 	Get Element Text
  -- Returns the *visible rendered text* of the element and all its descendants,
  -- whitespace-collapsed the same way a user sees it on screen (mirrors the
  -- JS `element.innerText` semantics).  Hidden elements contribute no text.
  -- Example: GET .../text  on <button>  Submit </button> → "Submit"
  --          GET .../text  on <p>Hello <span>world</span></p> → "Hello world"
  --
  -- BiDi: NodeProperties.nodeValue only holds the raw value of a *text node*,
  -- not the rendered text of an element. For innerText semantics use
  -- script.callFunction:
  --   script.callFunction {
  --     functionDeclaration: "(el) => el.innerText",
  --     arguments: [{ sharedId: <id> }]
  --   }
  -- Alternatively, serialize the node with NodeProperties.children and
  -- recursively concatenate nodeValue of visible text-node descendants —
  -- but this misses CSS-driven hiding, so script.callFunction is preferred.

GET 	/session/{session id}/element/{element id}/name 	Get Element Tag Name
  -- Returns the *lowercase HTML tag name* of the element (equivalent to the JS
  -- `element.tagName.toLowerCase()`).  Useful for asserting that you have
  -- matched the right kind of element before interacting with it.
  -- Example: GET .../name  on <INPUT type="text"> → "input"
  --          GET .../name  on <BUTTON>Click</BUTTON> → "button"
  --
  -- BiDi: This is the *only* endpoint with a direct structural equivalent.
  -- NodeProperties.localName in the script.NodeRemoteValue returned by
  -- browsingContext.locateNodes already contains the lowercase tag name —
  -- no extra round-trip needed.
  --   nodeRemoteValue.value.localName  ≡  "button"

GET 	/session/{session id}/element/{element id}/rect 	Get Element Rect
  -- Returns a JSON object describing the element's *bounding rectangle* in the
  -- viewport coordinate system: { x, y, width, height } (all in CSS pixels).
  -- x/y are the top-left corner relative to the document origin.
  -- Useful for checking visibility, position, or size without running JS.
  -- Example: GET .../rect  → {"x": 120.5, "y": 340.0, "width": 200.0, "height": 48.0}
  --
  -- BiDi: No direct message. Use script.callFunction:
  --   script.callFunction {
  --     functionDeclaration: "(el) => { const r = el.getBoundingClientRect();
  --                            return {x:r.x, y:r.y, width:r.width, height:r.height}; }",
  --     arguments: [{ sharedId: <id> }]
  --   }
  -- Returns a script.ObjectRemoteValue with number-typed entries.

GET 	/session/{session id}/element/{element id}/enabled 	Is Element Enabled
  -- Returns true/false indicating whether the element is currently *interactive*.
  -- An element is disabled (false) when it carries the HTML `disabled` attribute
  -- or is a form control inside a disabled <fieldset>.  Non-form elements (e.g.
  -- a plain <div>) always return true regardless of any aria-disabled attribute.
  -- Example: GET .../enabled  on <button disabled> → false
  --          GET .../enabled  on <input type="text"> → true
  --
  -- BiDi: No direct message. Use script.callFunction:
  --   script.callFunction {
  --     functionDeclaration: "(el) => !el.disabled",
  --     arguments: [{ sharedId: <id> }]
  --   }
  -- Note: NodeProperties.attributes may contain a "disabled" key, but only if
  -- the attribute is present in the HTML source — it won't reflect a <fieldset>
  -- disabled ancestor, making the JS property check more reliable.

GET 	/session/{session id}/element/{element id}/computedrole 	Get Computed Role
  -- Returns the *accessibility role* the browser has resolved for the element,
  -- taking both implicit (native HTML) roles and explicit `role=` attributes into
  -- account.  This is what an assistive technology actually sees, so it is more
  -- reliable for a11y assertions than checking the `role` attribute directly.
  -- Example: GET .../computedrole  on <button> → "button"
  --          GET .../computedrole  on <div role="alert"> → "alert"
  --          GET .../computedrole  on <div> (no role) → "generic"
  --
  -- BiDi: No direct message. Use script.callFunction via the Accessibility
  -- Object Model (AOM) property el.computedRole (Chrome 90+, Firefox preview):
  --   script.callFunction {
  --     functionDeclaration: "(el) => el.computedRole",
  --     arguments: [{ sharedId: <id> }]
  --   }
  -- NodeProperties.attributes["role"] only gives the *explicit* attribute value,
  -- missing all implicit roles from native HTML semantics.

GET 	/session/{session id}/element/{element id}/computedlabel 	Get Computed Label
  -- Returns the *accessible name* of the element as computed by the browser's
  -- accessible name and description computation algorithm (ARIA spec).  This
  -- aggregates aria-label, aria-labelledby, the element's own text content,
  -- associated <label> elements, alt text, title, placeholder, etc. — the same
  -- string a screen reader would announce.
  -- Example: GET .../computedlabel  on <button aria-label="Close dialog"> → "Close dialog"
  --          GET .../computedlabel  on <input id="x"><label for="x">Email</label> → "Email"
  --
  -- BiDi: No direct message. Use script.callFunction via the AOM property
  -- el.computedLabel (Chrome 90+):
  --   script.callFunction {
  --     functionDeclaration: "(el) => el.computedLabel",
  --     arguments: [{ sharedId: <id> }]
  --   }
  -- NodeProperties.attributes can surface "aria-label" directly, but that is
  -- only one of many labelling sources — it misses aria-labelledby (requires
  -- following an idref), inner text, <label for=...>, alt, title, etc.
  -- el.computedLabel is the only way to get the fully resolved accessible name
  -- without reimplementing the entire ARIA naming algorithm in the client.

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

-- | ARIA roles from https://www.w3.org/TR/wai-aria-1.2/#role_definitions
data AriaRole
  = Article
  | Banner
  | Button
  | Cell
  | Checkbox
  | ColumnHeader
  | Complementary
  | ContentInfo
  | Definition
  | Dialog
  | Figure
  | Form
  | Group
  | Heading
  | Img
  | Link
  | List
  | ListItem
  | Main
  | Navigation
  | Option
  | ProgressBar
  | Radio
  | Region
  | Row
  | RowHeader
  | Search
  | Separator
  | Slider
  | SpinButton
  | Status
  | Table
  | Term
  | Textbox
  deriving (Show, Eq, Ord, Enum, Bounded)

displayAriaRole :: AriaRole -> Text
displayAriaRole = toLower . pack . show

someBoolVal :: Bool
someBoolVal = True || False && (True || False) && not (False || True)

data MatchType = Full | Partial deriving (Show, Eq)

-- //*[contains(translate(@class, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz'), 'submit')]

-- | Best-effort XPath approximation of a BiDi accessibility locator.
--   Covers explicit @role attributes, implicit roles for common HTML elements,
--   and common accessible name sources. Cannot cover aria-labelledby,
--   label[for=...], or shadow DOM.
-- TODO: cover edge cases in execution
accessibilityToXPath :: Maybe AriaRole -> Maybe Text -> Maybe Text
accessibilityToXPath = \cases
  Nothing Nothing -> Nothing
  mRole mName -> Just $ "//*" <> rle mRole <> name mName
  where
    rle = maybe "" \r -> "[" <> implicitRoleXPath r <> " or @role='" <> displayAriaRole r <> "']"

    name = maybe "" \n ->
      "["
        <> intercalate
          " or "
          [ "@aria-label='" <> n <> "'",
            "@placeholder='" <> n <> "'",
            "@title='" <> n <> "'",
            "@alt='" <> n <> "'",
            "normalize-space(text())='" <> n <> "'"
          ]
        <> "]"

-- | Best-effort XPath approximation of a BiDi inner text locator.
--   Uses normalize-space(.) as a proxy for innerText. Case-insensitive
--   matching uses the translate() alphabet hack. maxDepth is approximated
--   via count(ancestor::*).
--   Visibility filtering: excludes elements with the HTML @hidden attribute,
--   @aria-hidden='true', or inline style display:none / visibility:hidden.
--   Does NOT catch hiding via CSS classes or inherited/cascaded styles —
--   only the BiDi innerText locator handles those correctly.
innerTextToXPath :: Text -> Maybe Bool -> Maybe MatchType -> Maybe JSUInt -> Text
innerTextToXPath val mIgnoreCase mMatchType mMaxDepth =
  "//*" <> depthPred <> "[" <> hiddenPred <> " and " <> textPred <> "]"
  where
    caseInsensitive = maybe False id mIgnoreCase
    fullMatch = mMatchType == Just Full

    normalisedText
      | caseInsensitive = "translate(normalize-space(.), '" <> upperAlpha <> "', '" <> lowerAlpha <> "')"
      | otherwise = "normalize-space(.)"

    matchVal
      | caseInsensitive = toLower val
      | otherwise = val

    textPred
      | fullMatch = normalisedText <> "='" <> matchVal <> "'"
      | otherwise = "contains(" <> normalisedText <> ", '" <> matchVal <> "')"

    -- Partial visibility filter: catches @hidden, aria-hidden, and inline styles only.
    -- Cannot detect hiding via CSS classes or ancestor cascade.
    hiddenPred =
      "not(@hidden)"
        <> " and not(@aria-hidden='true')"
        <> " and not(contains(@style,'display:none'))"
        <> " and not(contains(@style,'visibility:hidden'))"

    depthPred = maybe "" (\d -> "[count(ancestor::*)<=" <> pack (show d) <> "]") mMaxDepth

    upperAlpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    lowerAlpha = "abcdefghijklmnopqrstuvwxyz"

-- | Maps an ARIA role to an XPath predicate matching elements that have
--   that role implicitly (i.e. without an explicit role= attribute).
--   Based on the ARIA in HTML spec: https://www.w3.org/TR/html-aria/
implicitRoleXPath :: AriaRole -> Text
implicitRoleXPath =
  ("self::" <>) . \case
    Article -> "article"
    Banner -> "header[not(ancestor::article) and not(ancestor::section)]"
    Button -> "button or self::input[@type='button'] or self::input[@type='submit'] or self::input[@type='reset'] or self::input[@type='image'] or self::summary"
    Cell -> "td"
    Checkbox -> "input[@type='checkbox']"
    ColumnHeader -> "th[@scope='col' or not(@scope)]"
    Complementary -> "aside"
    ContentInfo -> "footer[not(ancestor::article) and not(ancestor::section)]"
    Definition -> "dd"
    Dialog -> "dialog"
    Figure -> "figure"
    Form -> "form[@aria-label or @aria-labelledby]"
    Group -> "fieldset or self::optgroup"
    Heading -> "h1 or self::h2 or self::h3 or self::h4 or self::h5 or self::h6"
    Img -> "img[@alt and string-length(@alt)>0]"
    Link -> "a[@href] or self::area[@href]"
    List -> "ul or self::ol"
    ListItem -> "li"
    Main -> "main"
    Navigation -> "nav"
    Option -> "option"
    ProgressBar -> "progress"
    Radio -> "input[@type='radio']"
    Region -> "section[@aria-label or @aria-labelledby]"
    Row -> "tr"
    RowHeader -> "th[@scope='row']"
    Search -> "search"
    Separator -> "hr"
    Slider -> "input[@type='range']"
    SpinButton -> "input[@type='number']"
    Status -> "output"
    Table -> "table"
    Term -> "dt"
    Textbox -> "input[not(@type) or @type='text' or @type='email' or @type='tel' or @type='url' or @type='search'] or self::textarea"

{- https://chatgpt.com/c/69ab3adc-94bc-8324-bf32-218569fcaf86
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

Performance tip (important)

For automation frameworks, inject the helper once per page:

window.__bidiIsVisible = bidiIsVisible

Then call:

__bidiIsVisible(element)

This avoids re-sending the function each time

-}
-- Looks like we need a GADT

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

  -- locators lieient by default ??

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

  -- default string treatment
  click submit_button
  set "given_name" "John"

  -- globs by default

  welcomeMessage = hasText "Hello * welcome to Myer"

  hasText' [NoGlobs, CaseSensitive, FullMatch] "Hello * welcome to Myer"

  hasParent Locator
  hasDirectParent Locator

  -- an edit type which is an id and a value and has a monoid instance

  Edit type

  set EDIT
  set $
   "given_name" ~> "John"
   <> "last_name" "Walker"
   <> byPlaceholder "Title" "Sir"

-- I doubt this would work ??
-- use GADTs ? or just a parameterised data type wiith one param type for Locators and another for values

perhaps a data type with selector -> a -> m () that could be partially applied

  ```

  todo:
  - work out how user would deal with name collisions eg Button tag and Button role
    - typeclass ?
    - smart constructors

  -}
