# Mega Forma — Locator Test Reference

`megaforma.html` is a large synthetic test page designed to exercise locator strategies, edge cases, and visibility semantics for both HTTP WebDriver and WebDriver BiDi automation.

---

## Table of Contents

1. [High-Level Structure](#1-high-level-structure)
2. [Input Types Present](#2-input-types-present)
3. [Label Strategies](#3-label-strategies)
4. [ARIA Role Coverage](#4-aria-role-coverage)
5. [Hidden Elements Showcase](#5-hidden-elements-showcase)
6. [Same-Text Duplicates](#6-same-text-duplicates)
7. [Duplicate Sections (Parent Disambiguation)](#7-duplicate-sections-parent-disambiguation)
8. [iFrames](#8-iframes)
9. [Multi-Paragraph Text Blocks](#9-multi-paragraph-text-blocks)
10. [auto-id Convention](#10-auto-id-convention)
11. [Locator Happy Paths](#11-locator-happy-paths)
12. [Locator Edge Cases](#12-locator-edge-cases)

---

## 1. High-Level Structure

| `auto-id`             | Element / Role            | Purpose                                                 |
| --------------------- | ------------------------- | ------------------------------------------------------- |
| `hdr-main`            | `<header>` (banner)       | Page banner landmark                                    |
| `nav-main`            | `<nav>` (navigation)      | Main navigation links                                   |
| `main-content`        | `<main>`                  | Main content landmark                                   |
| `frm-mega`            | `<form>`                  | The single mega form                                    |
| `sec-personal`        | `<section>`               | Personal info — various label strategies                |
| `sec-names`           | `<section>`               | 6 labels with same prefix/suffix                        |
| `sec-contact`         | `<section>`               | Contact details                                         |
| `sec-preferences`     | `<section>`               | Checkboxes, radios, switch, billing toggle              |
| `sec-account`         | `<section>`               | Full input-type coverage, select, range, file…          |
| `sec-message`         | `<section>`               | Textarea (visible + hidden same class), contenteditable |
| `sec-aria`            | `<section>`               | Exhaustive ARIA role showcase                           |
| `sec-text`            | `<section>`               | Multi-paragraph divs, inline semantic elements          |
| `sec-dup-a`           | `<div>`                   | Duplicate section A (parent disambiguation)             |
| `sec-dup-b`           | `<div>`                   | Duplicate section B                                     |
| `sec-hidden`          | `<section>`               | 8 distinct hiding strategies                            |
| `sec-payment`         | `<section>`               | Payment iframe (always visible)                         |
| `wrap-delivery-frame` | `<div>`                   | Delivery iframe wrapper (hidden by default)             |
| `aside-help`          | `<aside>` (complementary) | Keyboard hints                                          |
| `ftr-main`            | `<footer>` (contentinfo)  | Footer landmark                                         |

---

## 2. Input Types Present

All standard HTML input types appear in the form.

| `auto-id`                           | `type`               | Initial value          | Section     |
| ----------------------------------- | -------------------- | ---------------------- | ----------- |
| `edt-given-name`                    | text                 | Jane                   | Personal    |
| `edt-family-name`                   | text                 | Smith                  | Personal    |
| `edt-nickname`                      | text                 | Jay                    | Personal    |
| `edt-dob`                           | date                 | 1990-06-15             | Personal    |
| `edt-middle-name`                   | text                 | _(empty)_              | Personal    |
| `edt-honorific`                     | text                 | Dr                     | Personal    |
| `edt-demo-id`                       | text                 | demo value             | Personal    |
| `edt-bio`                           | contenteditable      | text                   | Personal    |
| `edt-name-first` … `edt-name-legal` | text                 | see below              | Names       |
| `edt-email`                         | email                | jane.smith@example.com | Contact     |
| `edt-phone`                         | tel                  | +61 400 000 000        | Contact     |
| `edt-website`                       | url                  | https://example.com    | Contact     |
| `edt-search`                        | search               | webdriver              | Contact     |
| `chk-news-tech`                     | checkbox             | checked                | Preferences |
| `chk-news-arts`                     | checkbox             | checked                | Preferences |
| `rdo-contact-email`                 | radio                | checked                | Preferences |
| `btn-marketing-switch`              | button (role=switch) | aria-checked=true      | Preferences |
| `chk-same-address`                  | checkbox             | checked                | Preferences |
| `sel-country`                       | select               | Australia              | Account     |
| `sel-languages`                     | select[multiple]     | English, French        | Account     |
| `edt-city`                          | text + datalist      | Sydney                 | Account     |
| `edt-age-range`                     | range                | 35                     | Account     |
| `edt-fav-color`                     | color                | #3366cc                | Account     |
| `edt-start-date`                    | date                 | 2026-01-01             | Account     |
| `edt-meeting-time`                  | time                 | 09:00                  | Account     |
| `edt-appointment`                   | datetime-local       | 2026-03-20T14:30       | Account     |
| `edt-birth-month`                   | month                | 1990-06                | Account     |
| `edt-report-week`                   | week                 | 2026-W12               | Account     |
| `edt-score`                         | number               | 42                     | Account     |
| `edt-password`                      | password             | S3cr3tP@ss             | Account     |
| `edt-avatar`                        | file                 | _(none)_               | Account     |
| `hdn-session-token`                 | hidden               | abc123xyz              | Account     |
| `prg-profile`                       | `<progress>`         | 72                     | Account     |
| `mtr-storage`                       | `<meter>`            | 65                     | Account     |
| `out-age-range`                     | `<output>`           | 35                     | Account     |
| `edt-notes-visible`                 | textarea             | text                   | Message     |
| `edt-notes-hidden`                  | textarea (hidden)    | text                   | Message     |
| `edt-rich-edit`                     | contenteditable      | HTML                   | Message     |
| `edt-global-search`                 | search               | _(empty)_              | ARIA        |
| `edt-vis-ref`                       | text                 | I am fully visible     | Hidden      |
| `btn-submit`                        | button[submit]       | —                      | Submit      |
| `btn-submit-input`                  | input[submit]        | —                      | Submit      |
| `btn-image-submit`                  | input[image]         | —                      | Submit      |
| `btn-reset`                         | button[reset]        | —                      | Submit      |
| `btn-reset-input`                   | input[reset]         | —                      | Submit      |

### Iframe inputs (`iframe-payment`)

| `auto-id`          | `type`   | Initial value       |
| ------------------ | -------- | ------------------- |
| `edt-card-holder`  | text     | Jane Smith          |
| `edt-card-number`  | text     | 4111 1111 1111 1111 |
| `edt-card-expiry`  | text     | 12/28               |
| `edt-card-cvv`     | password | 123                 |
| `sel-payment-type` | select   | Visa                |
| `chk-save-card`    | checkbox | unchecked           |

### Iframe inputs (`iframe-delivery`) — hidden initially

| `auto-id`              | `type`   | Initial value |
| ---------------------- | -------- | ------------- |
| `edt-del-street`       | text     | 42 Test Lane  |
| `edt-del-suburb`       | text     | Newtown       |
| `sel-del-state`        | select   | NSW           |
| `edt-del-postcode`     | text     | 2042          |
| `sel-del-country`      | select   | Australia     |
| `edt-del-instructions` | textarea | Leave at door |

---

## 3. Label Strategies

Six strategies are used across the form. Each tests a different branch of the accessible-name computation algorithm.

| Strategy                | Element pattern                                             | `auto-id` example                     | Locatable by                                                                                                         |
| ----------------------- | ----------------------------------------------------------- | ------------------------------------- | -------------------------------------------------------------------------------------------------------------------- |
| **1. Explicit for/id**  | `<label for="id">` + `<input id="id">`                      | `lbl-given-name` / `edt-given-name`   | CSS `label[for="given-name"]`, CSS `input#given-name`, XPath `//label[@for='given-name']`, BiDi accessibility `name` |
| **2. Implicit wrap**    | `<label>text<input></label>`                                | `lbl-family-name` / `edt-family-name` | BiDi accessibility `name`, XPath ancestor label text                                                                 |
| **3. aria-label**       | `<input aria-label="Nickname">`                             | `edt-nickname`                        | CSS `[aria-label="Nickname"]`, BiDi accessibility `name`                                                             |
| **4. aria-labelledby**  | `<span id="lbl-dob">` + `<input aria-labelledby="lbl-dob">` | `lbl-dob` / `edt-dob`                 | BiDi accessibility `name` (resolves idref), XPath fails the reverse lookup                                           |
| **5. Placeholder only** | `<input placeholder="Middle name…">`                        | `edt-middle-name`                     | CSS `[placeholder="Middle name (optional)"]`, limited BiDi name support                                              |
| **6. Title only**       | `<input title="Title or honorific…">`                       | `edt-honorific`                       | CSS `[title~="honorific"]`, BiDi name fallback                                                                       |

### CSS `for`/`id` demo pair

`label[for="demo-id"]` and `input#demo-id` both target the same field (`auto-id="lbl-demo-id"` / `auto-id="edt-demo-id"`).

---

## 4. ARIA Role Coverage

### Roles from native HTML (implicit)

| Element                     | Implicit role                             | `auto-id` example   |
| --------------------------- | ----------------------------------------- | ------------------- |
| `<header>`                  | `banner`                                  | `hdr-main`          |
| `<nav>`                     | `navigation`                              | `nav-main`          |
| `<main>`                    | `main`                                    | `main-content`      |
| `<footer>`                  | `contentinfo`                             | `ftr-main`          |
| `<aside>`                   | `complementary`                           | `aside-help`        |
| `<form>`                    | `form`                                    | `frm-mega`          |
| `<search>`                  | `search`                                  | `srch-widget`       |
| `<article>`                 | `article`                                 | `art-announcement`  |
| `<section aria-labelledby>` | `region`                                  | `sec-personal`      |
| `<details>` / `<summary>`   | `group` / `button`                        | `acc-faq`           |
| `<figure>`                  | `figure`                                  | `fig-demo`          |
| `<table>`                   | `table`                                   | `tbl-summary`       |
| `<th scope="col">`          | `columnheader`                            | `th-field`          |
| `<th scope="row">`          | `rowheader`                               | _(see grid)_        |
| `<td>`                      | `cell`                                    | `td-val-given-name` |
| `<ul>` / `<ol>`             | `list`                                    | `lst-shortcuts`     |
| `<li>`                      | `listitem`                                | `li-shortcut-tab`   |
| `<a href>`                  | `link`                                    | `lnk-nav-personal`  |
| `<button>`                  | `button`                                  | `btn-submit`        |
| `<input type=text>`         | `textbox`                                 | `edt-given-name`    |
| `<input type=search>`       | `searchbox`                               | `edt-search`        |
| `<input type=checkbox>`     | `checkbox`                                | `chk-news-tech`     |
| `<input type=radio>`        | `radio`                                   | `rdo-contact-email` |
| `<select>`                  | `listbox`                                 | `sel-country`       |
| `<option>`                  | `option`                                  | _(in sel-country)_  |
| `<input type=range>`        | `slider`                                  | `edt-age-range`     |
| `<input type=number>`       | `spinbutton`                              | `edt-score`         |
| `<progress>`                | `progressbar`                             | `prg-profile`       |
| `<fieldset>`                | `group`                                   | `fld-newsletter`    |
| `<legend>`                  | (names the group)                         | `lgd-newsletter`    |
| `<h1>`–`<h6>`               | `heading` (level N)                       | `hdg-main-title`    |
| `<img alt="…">`             | `img`                                     | `img-demo`          |
| `<dl>` / `<dt>` / `<dd>`    | `definition list` / `term` / `definition` | `dl-glossary`       |

### Roles via explicit `role` attribute

| `role` value       | `auto-id`                  | Notes                              |
| ------------------ | -------------------------- | ---------------------------------- |
| `tablist`          | `tablist-main`             | Contains `tab` children            |
| `tab`              | `tab-overview` etc.        | `aria-selected`, `aria-controls`   |
| `tabpanel`         | `panel-overview` etc.      | `aria-labelledby`                  |
| `alert`            | `alrt-validation`          | `aria-live="assertive"`            |
| `status`           | `sts-save`                 | `aria-live="polite"`               |
| `log`              | `div-log`                  | Chat-style live region             |
| `timer`            | `div-timer`                | `aria-live="off"`                  |
| `marquee`          | `div-marquee`              | Rotational live region             |
| `tooltip`          | `tip-info`                 | Revealed on hover                  |
| `dialog`           | `dlg-confirm`              | `aria-modal`, `aria-labelledby`    |
| `tree`             | `tree-categories`          | Contains `treeitem`                |
| `treeitem`         | `tree-item-tech`           | `aria-expanded`                    |
| `group`            | _(inside tree)_            | Child `ul` of `treeitem`           |
| `menu`             | `mnu-actions`              | Popup action menu                  |
| `menuitem`         | `mnu-item-edit`            | Plain action                       |
| `menuitemcheckbox` | `mnu-item-archive`         | `aria-checked`                     |
| `menuitemradio`    | `mnu-item-private`         | `aria-checked`                     |
| `grid`             | `grid-perms`               | Permissions table                  |
| `row`              | `grid-row-hdr` etc.        | Inside grid                        |
| `columnheader`     | `grid-col-feature` etc.    | Header row cells                   |
| `rowheader`        | `grid-rh-docs` etc.        | Row label cells                    |
| `gridcell`         | `grid-cell-docs-read` etc. | Data cells                         |
| `listbox`          | `lst-skills`               | Custom multi-select                |
| `option`           | `opt-haskell` etc.         | `aria-selected`                    |
| `progressbar`      | `prg-upload`               | `aria-valuenow`                    |
| `slider`           | `sld-volume`               | Div with range semantics           |
| `spinbutton`       | `edt-score`                | Explicit on `input[type=number]`   |
| `switch`           | `btn-marketing-switch`     | Toggle with `aria-checked`         |
| `searchbox`        | `edt-search`               | Explicit on `input[type=search]`   |
| `textbox`          | `edt-bio`, `edt-rich-edit` | On contenteditable divs            |
| `button`           | `btn-span-role`            | `<span role="button">`             |
| `button`           | `btn-link-role`            | `<a role="button">`                |
| `img`              | `img-div-role`             | `<div role="img">`                 |
| `presentation`     | `tbl-layout`               | Layout table, semantics suppressed |
| `none`             | `img-decorative`           | Decorative image                   |
| `math`             | `div-math`                 | Mathematical expression            |
| `application`      | `div-app`                  | Script-managed region              |

---

## 5. Hidden Elements Showcase

All eight variants are in `sec-hidden`. The visible reference input (`edt-vis-ref`) is the control.

| `auto-id`                              | Hiding mechanism                  | In DOM? | In layout?         | In AT tree? | Visible to user?    |
| -------------------------------------- | --------------------------------- | ------- | ------------------ | ----------- | ------------------- |
| `edt-vis-ref`                          | _(not hidden)_                    | ✓       | ✓                  | ✓           | ✓                   |
| `para-aria-hidden` / `edt-aria-hidden` | `aria-hidden="true"`              | ✓       | ✓                  | ✗           | ✓                   |
| `fg-display-none`                      | `style="display:none"`            | ✓       | ✗                  | ✗           | ✗                   |
| `fg-css-none`                          | CSS class `.hidden-display`       | ✓       | ✗                  | ✗           | ✗                   |
| `fg-vis-hidden`                        | `style="visibility:hidden"`       | ✓       | ✓ (space reserved) | ✗           | ✗                   |
| `fg-css-vis-hidden`                    | CSS class `.hidden-visibility`    | ✓       | ✓ (space reserved) | ✗           | ✗                   |
| `fg-html-hidden`                       | HTML `hidden` attribute           | ✓       | ✗                  | ✗           | ✗                   |
| `fg-opacity-zero`                      | `style="opacity:0"`               | ✓       | ✓                  | ✓           | ✗ (invisible)       |
| `fg-offscreen`                         | `position:absolute; left:-9999px` | ✓       | ✓ (off-viewport)   | ✓           | ✗ (out of viewport) |

### Locator behaviour differences

- **XPath `text()`** finds `aria-hidden` and `opacity:0` text because both are in the DOM.
- **`display:none` / `hidden` attribute** — elements are not rendered; classic WebDriver `isDisplayed` returns false; BiDi `elementFromPoint` returns null.
- **`visibility:hidden`** — element occupies space; `isDisplayed` returns false; BiDi hybrid function detects it via `getComputedStyle`.
- **`opacity:0`** — WebDriver `isDisplayed` may return **true** (wrong); BiDi `elementFromPoint` returns **null** (more accurate).
- **Off-screen** — `isDisplayed` may return true; BiDi viewport-intersection check returns false.
- **`aria-hidden`** — BiDi accessibility locator skips these; CSS / XPath / `querySelectorAll` still finds them.

---

## 6. Same-Text Duplicates

Six labels in `sec-names` share an identical prefix (`Please enter your`) and suffix (`Name here`) with a different middle word.

| `auto-id` label      | Full text                               |
| -------------------- | --------------------------------------- |
| `lbl-name-first`     | `Please enter your First Name here`     |
| `lbl-name-last`      | `Please enter your Last Name here`      |
| `lbl-name-middle`    | `Please enter your Middle Name here`    |
| `lbl-name-nick`      | `Please enter your Nick Name here`      |
| `lbl-name-preferred` | `Please enter your Preferred Name here` |
| `lbl-name-legal`     | `Please enter your Legal Name here`     |

### Test scenarios

| Scenario               | Locator / check                                               |
| ---------------------- | ------------------------------------------------------------- |
| Exact match            | only one result for `"Please enter your Legal Name here"`     |
| Partial/contains match | 6 results for contains `"enter your"… "Name here"`            |
| Case-insensitive match | `"please enter your first name here"` should hit same element |
| Starts-with            | 6 results for `starts-with(., "Please enter your")`           |
| Ends-with              | 6 results for `ends-with(., "Name here")`                     |
| Word-boundary          | `First` vs `First` vs `Preferred` distinguishes results       |

---

## 7. Duplicate Sections (Parent Disambiguation)

`sec-dup-a` and `sec-dup-b` contain identical internal markup — same labels, button text, list items, and CSS classes — but different parent `id` attributes.

| Element pattern    | In A                       | In B                         |
| ------------------ | -------------------------- | ---------------------------- |
| Input label        | `"Shared Input Label"`     | `"Shared Input Label"`       |
| Input value        | `"Value in section A"`     | `"Value in section B"`       |
| Button text        | `"Shared Button Text"`     | `"Shared Button Text"`       |
| Status badge class | `.status-indicator.active` | `.status-indicator.inactive` |
| List items         | Item One, Two, Three       | Item One, Two, Three         |

### Disambiguation strategies

| Goal                              | CSS selector                           | XPath                                                                    |
| --------------------------------- | -------------------------------------- | ------------------------------------------------------------------------ |
| Button in A only                  | `#section-duplicate-a button`          | `//div[@id='section-duplicate-a']//button`                               |
| Input in B only                   | `#section-duplicate-b input.dup-input` | `//div[@id='section-duplicate-b']//input`                                |
| Active status only                | `.status-indicator.active`             | `//*[contains(@class,'active') and contains(@class,'status-indicator')]` |
| By `auto-id` regardless of parent | `[auto-id="btn-dup-a"]`                | `//*[@auto-id='btn-dup-a']`                                              |

---

## 8. iFrames

| Frame `auto-id`                                  | Initially visible? | Trigger                    | Contents                                                                   |
| ------------------------------------------------ | ------------------ | -------------------------- | -------------------------------------------------------------------------- |
| `iframe-payment`                                 | **Yes**            | —                          | Cardholder name, card number, expiry, CVV, type select, save-card checkbox |
| `iframe-delivery` (inside `wrap-delivery-frame`) | **No**             | Uncheck `chk-same-address` | Street, suburb, state, postcode, country, instructions textarea            |

### BiDi vs HTTP interaction

**BiDi (no context switching needed):**

```
browsingContext.getTree({ root: main_ctx })  →  child_ctx (payment frame)
browsingContext.locateNodes({ context: child_ctx, locator: { type: "css", value: "#card-number" } })
```

**HTTP WebDriver (stateful switch required):**

```
POST /session/{id}/element  { using: "css selector", value: "#iframe-payment" }
POST /session/{id}/frame    { id: { "element-6066…": "elem-xyz" } }
POST /session/{id}/element  { using: "css selector", value: "#card-number" }
…interact…
POST /session/{id}/frame    { id: null }   ← must restore
```

---

## 9. Multi-Paragraph Text Blocks

Three divs in `sec-text` split content across `<p>` children.

| `auto-id` div       | Paragraphs                      | Purpose                  |
| ------------------- | ------------------------------- | ------------------------ |
| `div-bio-content`   | `para-bio-1` … `para-bio-3`     | Basic split              |
| `div-terms-content` | `para-terms-1` … `para-terms-3` | One highlighted para     |
| `div-mixed-inline`  | Multiple                        | Inline semantic elements |

### Locator implications

| What you want                     | XPath                                                | BiDi innerText locator      |
| --------------------------------- | ---------------------------------------------------- | --------------------------- |
| Inner text of a single `<p>`      | `//p[@auto-id='para-bio-2']`                         | exact/partial on that `<p>` |
| Inner text spanning the whole div | `normalize-space(//div[@auto-id='div-bio-content'])` | `contains` on the `<div>`   |
| Text only in `<strong>`           | `//strong[text()='bold']`                            | partial on the `<strong>`   |
| Highlighted paragraph only        | `//p[contains(@class,'highlight-para')]`             | CSS `p.highlight-para`      |

**Note:** `innerText` strips elements hidden via `display:none` and reflects line-break whitespace; `XPath text()` returns raw DOM text nodes without that normalisation. They will differ for these elements.

---

## 10. `auto-id` Convention

Every interactable element and label carries an `auto-id` custom attribute.

| Prefix                         | Element type                                             |
| ------------------------------ | -------------------------------------------------------- |
| `edt-`                         | Editable field (input, textarea, contenteditable)        |
| `lbl-`                         | Label or labelling element                               |
| `btn-`                         | Button (`<button>`, `<input type="submit/reset/image">`) |
| `chk-`                         | Checkbox                                                 |
| `rdo-`                         | Radio button                                             |
| `sel-`                         | Select / listbox                                         |
| `opt-`                         | Option within a listbox                                  |
| `frm-`                         | Form                                                     |
| `sec-`                         | Section or major div                                     |
| `hdg-`                         | Heading                                                  |
| `nav-`                         | Navigation landmark                                      |
| `hdr-`                         | Header landmark                                          |
| `ftr-`                         | Footer landmark                                          |
| `lnk-`                         | Anchor/link                                              |
| `fld-` / `lgd-`                | Fieldset / Legend                                        |
| `tbl-` / `tr-` / `td-` / `th-` | Table / row / cell                                       |
| `lst-` / `li-`                 | List / list item                                         |
| `dlg-`                         | Dialog                                                   |
| `tab-` / `panel-`              | Tab / tab panel                                          |
| `mnu-` / `mnu-item-`           | Menu / menu item                                         |
| `alrt-` / `sts-`               | Alert / status live region                               |
| `tip-`                         | Tooltip                                                  |
| `prg-` / `mtr-` / `out-`       | Progress / meter / output                                |
| `hdn-`                         | Hidden input                                             |
| `art-`                         | Article                                                  |
| `fig-`                         | Figure                                                   |
| `img-`                         | Image                                                    |
| `spn-`                         | Span                                                     |
| `div-`                         | Generic div                                              |
| `para-`                        | Paragraph                                                |
| `bq-`                          | Blockquote                                               |
| `pre-`                         | Pre/code block                                           |
| `acc-`                         | Accordion (details/summary)                              |
| `aside-`                       | Aside landmark                                           |
| `srch-`                        | Search landmark                                          |
| `sld-`                         | Custom slider                                            |
| `iframe-` / `wrap-`            | Iframe / wrapper div                                     |
| `wrap-`                        | Generic wrapper div                                      |

CSS locator: `[auto-id="btn-submit"]`  
XPath locator: `//*[@auto-id='btn-submit']`

---

## 11. Locator Happy Paths

### By CSS id

```css
input#given-name          /* unique CSS id on input */
label[for="given-name"]   /* label associated via for */
input#demo-id             /* explicit CSS id demo */
```

### By CSS attribute

```css
[auto-id="edt-email"]
[aria-label="Nickname"]
[placeholder="Middle name (optional)"]
[title~="honorific"]
[role="switch"]
```

### By XPath text

```xpath
//label[normalize-space(.)='Given Name']
//button[.='Submit']
//li[@role='option' and .='Haskell']
```

### By ARIA accessibility locator (BiDi)

```json
{ "type": "accessibility", "value": { "role": "textbox", "name": "Given Name" } }
{ "type": "accessibility", "value": { "role": "button", "name": "Submit the mega form" } }
{ "type": "accessibility", "value": { "role": "checkbox", "name": "Technology" } }
{ "type": "accessibility", "value": { "role": "tab", "name": "Details" } }
```

### By BiDi innerText locator

```json
{ "type": "innerText", "value": "Please enter your Legal Name here" }
{ "type": "innerText", "value": "Overview", "matchType": "full" }
{ "type": "innerText", "value": "biography", "matchType": "partial" }
```

---

## 12. Locator Edge Cases

### 12.1 Deduplicating by parent section

Both duplicate sections have `"Shared Input Label"` and `"Shared Button Text"`. A naïve text locator returns two results; scope them:

```css
#section-duplicate-a [auto-id="edt-dup-input-a"]
#section-duplicate-b button.dup-btn
```

BiDi — locate relative to a context node:

```
locateNodes({ context: main_ctx, locator: css "#section-duplicate-a button" })
```

### 12.2 Detecting hidden elements

| Locator type                           | Finds `display:none`? | Finds `aria-hidden`? | Finds `opacity:0`?  |
| -------------------------------------- | --------------------- | -------------------- | ------------------- |
| CSS selector                           | Yes                   | Yes                  | Yes                 |
| XPath                                  | Yes                   | Yes                  | Yes                 |
| BiDi `css` / `xpath` locator           | Yes                   | Yes                  | Yes                 |
| BiDi accessibility locator             | **No**                | **No**               | **No**              |
| WebDriver `isDisplayed`                | No                    | Yes _(displayed)_    | _browser-dependent_ |
| BiDi `script.evaluate` `bidiIsVisible` | No                    | Yes                  | No (opacity check)  |

### 12.3 visible innerText vs XPath `text()`

For `div-bio-content` the concatenated text across all three `<p>` children is:

> "This is the first paragraph… split across… Each `<p>` is a child…"

- **XPath `text()` on the `<div>`** returns nothing (text is in child nodes, not direct text children).
- **XPath `normalize-space(.)`** returns the concatenated text without element boundaries.
- **BiDi `innerText` locator** matches the computed rendered text and respects `display:none` children.
- **`notes-area-hidden`** textarea is hidden via CSS class. `text()` in XPath still finds its content; BiDi `innerText` on a hidden element returns an empty string.

### 12.4 aria-labelledby resolution chain

`edt-dob` has `aria-labelledby="lbl-dob"`.

- BiDi accessibility locator `{ role: "textbox", name: "Date of Birth" }` resolves the idref ✓
- XPath `//input[@aria-labelledby='lbl-dob']` matches the attribute ✓
- XPath `//input[accessible-name = 'Date of Birth']` — **no such XPath function** — cannot resolve at XPath level ✗

### 12.5 6-label partial vs exact match

XPath `contains` on label text:

```xpath
//label[contains(., 'Name here')]          /* returns all 6 */
//label[. = 'Please enter your Nick Name here']  /* returns exactly 1 */
```

XPath `translate()` case-insensitive:

```xpath
//label[translate(., 'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
                     'abcdefghijklmnopqrstuvwxyz')
        = 'please enter your first name here']
```

BiDi innerText `ignoreCase`:

```json
{ "type": "innerText", "value": "please enter your first name here",
  "ignoreCase": true, "matchType": "full" }
```

### 12.6 iFrame element access

Locating `#card-number` without first entering the payment frame context will fail. In HTTP WebDriver always switch in and restore. In BiDi always pass the child browsing context ID.

### 12.7 Semantics suppressed by `role="none"` / `role="presentation"`

`img-decorative` (`<img role="none">`) and `tbl-layout` (`<table role="presentation">`):

- BiDi accessibility locator will **not** find them by role.
- CSS and XPath locate them normally by tag/attribute.

### 12.8 Switch vs checkbox

`btn-marketing-switch` has `role="switch"` and `aria-checked`. Native checkboxes have `role="checkbox"` and `checked` property.

- Accessibility locator `{ role: "switch" }` only matches the switch button, not the checkboxes.
- `aria-checked` is the ARIA state; the property `checked` on `<input type=checkbox>` is the DOM property — they are distinct.

### 12.9 Contenteditable as textbox

`edt-bio` and `edt-rich-edit` are `<div role="textbox" contenteditable>`.

- They have no `value` property; their text content is in child DOM nodes.
- `input()` / `clear()` WebDriver commands do **not** work on them; use `send_keys` or BiDi `input.insertText`.
- Their accessible name comes from `aria-labelledby` (resolves the span), not from a `<label for>` association.

### 12.10 Opacity-zero visible vs invisible discrepancy

`edt-opacity-zero` is wrapped in `style="opacity:0"`.

- Classic WebDriver `isDisplayed` may return **true** (element has a layout box and is in the viewport).
- BiDi `document.elementFromPoint` at the element's centre returns **null** (another element is on top, or the transparent element is skipped).
- The BiDi hybrid `bidiIsVisible` function handles this correctly via the hit-test.
