# BiDi vs HTTP WebDriver: Interacting with Elements in Child iframes

**Scenario:** On main page with a child `<iframe>`, edit an element with id `firstName` inside the child frame.

---

## BiDi — no switching, context IDs are stable handles

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

---

## HTTP WebDriver — stateful switch required

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

---

## Key Difference

BiDi contexts are **first-class persistent handles** so you just address the child context directly. HTTP WebDriver has a single implicit "current frame" cursor that you have to move around and restore — easy to leak state if something throws between switch-in and switch-back.
