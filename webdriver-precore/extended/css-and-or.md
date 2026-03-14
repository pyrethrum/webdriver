css-depth-limiting.md

## CSS Depth Limiting

**Q:** If I have a complex CSS query is there a way to limit depth?

**A:** In CSS, there is **no native way to limit depth** directly.

**Indirect Methods:**

1. **Child Combinator Chain:**
```css
.parent > .child              /* depth 1 */
.parent > * > .child          /* depth 2 */
.parent > * > * > .child      /* depth 3 */
```

2. **:not() with Descendant Selectors:**
```css
.container .item:not(.container .container .item)
```

3. **JavaScript (Post-Query Filtering):**
```javascript
document.querySelectorAll('.parent .target').forEach(el => {
  let depth = 0;
  let node = el;
  while (node.parentElement && !node.parentElement.matches('.parent')) {
    depth++;
    node = node.parentElement;
  }
  if (depth <= 3) {
    // use this element
  }
});
```

4. **Use XPath Instead (Recommended):**
```xpath
//*[@class='parent']//*[@class='target'][count(ancestor::*) <= 3]
```

In Haskell:
```haskell
-- CSS: no depth control
css ".parent .target"

-- XPath: with depth control
xpath "//*[contains(@class,'parent')]//*[contains(@class,'target')][count(ancestor::*) <= 3]"
```

**Recommendation:** If you need depth limiting, use XPath or add a JavaScript-based post-filter function to your BiDi locator strategy.

---

## Summary

Key decisions made:
1. Adopted `Foo.Internal` module naming convention
2. Removed intermediate `Tags.Internal` module in favor of direct tag constructors
3. Simplified tag locators to use plain text values
4. Identified XPath as the solution for depth-limited queries

Project state: Compiles successfully with all tag locators implemented.

---

In CSS, you can make partial attribute selectors case-insensitive by adding the i flag after the value.
Syntax
Css[attribute*="value" i] { ... }


*= → matches if the attribute contains the given substring.
i → makes the match case-insensitive.


Example
Css/* Selects elements whose title contains "hello" (case-insensitive) */
[title*="hello" i] {
  color: red;
}

This will match:
Html<p title="Hello World">Matched</p>
<p title="HELLO WORLD">Matched</p>
<p title="heLLo there">Matched</p>


Other partial match operators with i



Operator
Meaning
Example




*=
contains
[attr*="val" i]


^=
starts with
[attr^="val" i]


$=
ends with
[attr$="val" i]

---

✅ Browser Support:
The i flag for attribute selectors is supported in all modern browsers (Chrome 49+, Firefox 47+, Safari 9+, Edge 79+). It is not supported in IE.

If you want, I can give you a full working HTML + CSS demo showing all partial case-insensitive selectors in action.
Do you want me to prepare that?

/* Matches any element whose class attribute contains "menu" (case-insensitive) */
[class*="menu" i] {
  background-color: yellow;
}
How it works:
[class*="menu"] → Matches if "menu" appears anywhere in the class attribute.
i → Makes the match case-insensitive (menu, Menu, MENU, etc.).
This works in modern browsers (CSS4 selectors).
Other partial match operators:
Operator	Meaning	Example
*=	Contains substring	[class*="menu" i]
^=	Starts with	[class^="menu" i]
$=	Ends with	[class$="menu" i]
~=	Contains whole word (space-separated)	[class~="menu" i]
Example HTML:
Html

Copy code
<div class="MainMenu">Main Menu</div>
<div class="submenu">Sub Menu</div>
<div class="MENU-item">Menu Item</div>
Result:
Css

Copy code
[class*="menu" i] {
  color: red;
}
This will style all three divs above, regardless of case.

If you want, I can also give you a JavaScript-based fallback for older browsers that don’t support the i flag in CSS selectors.
Do you want me to include that?


