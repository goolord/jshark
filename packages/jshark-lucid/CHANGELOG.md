# Changelog

## 0.1.0.0

* First release. See the [repository changelog](https://github.com/goolord/jshark/blob/master/CHANGELOG.md)
  for the full history of this package and the rest of the monorepo.
* Templates are validated before rendering: `templateErrors` returns
  structured `TemplateError`s (element path + message) for an orphan
  modifier or a child inside a void element, and `renderInto`/
  `renderFragment` throw the first one. Modifiers apply to their enclosing
  element and are hoisted ahead of its children.
