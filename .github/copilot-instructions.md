---
applyTo: '*.hs, *.lhs'
---
Provide project context and coding guidelines that AI should follow when generating code, answering questions, or reviewing changes.

# Haskell Coding Style Guidelines

## Code Style
- Use 2-space indentation
- Prefer explicit type signatures

## Patterns to Follow
- Prefer `(&)` for pipeline-style operations
- Use record syntax with OverloadedRecordDot
- Use `maybe`, `either`, `fromMaybe` rather than explicit case statements for `Maybe` and `Either`
- Use `LambdaCase`

## Avoid
- String type (prefer Text)
- Nested if-then-else (use case or guards)

