# Personal coding conventions

These rules apply across every project I work on.

- **Don't use `empty()` or `isset()` in PHP.** For arrays use `count($x) === 0` / `$x === []`; for keys use `array_key_exists($key, $arr)` or null-coalescing `$arr[$key] ?? $default`; for object properties prefer typed nullable properties with `?? null` access.
- **Don't add `Co-Authored-By` (or any other authorship trailer) to git commits.** Commit messages should match the project's existing style — no AI-attribution footers, no "Generated with" trailers, no co-author lines unless I explicitly ask for them.
