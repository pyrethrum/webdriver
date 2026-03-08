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
