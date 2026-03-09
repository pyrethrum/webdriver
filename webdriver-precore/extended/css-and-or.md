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