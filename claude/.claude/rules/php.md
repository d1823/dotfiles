---
paths:
  - "**/*.php"
---

# Rules

- **Don't use `empty()` or `isset()` in PHP.** For arrays use `count($x) === 0` / `$x === []`; for keys use `array_key_exists($key, $arr)` or null-coalescing `$arr[$key] ?? $default`; for object properties prefer typed nullable properties with `?? null` access.
