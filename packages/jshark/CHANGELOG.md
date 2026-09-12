# Changelog

## 0.1.0.0 (2026-09-11)

* First version.
* See the [repository changelog](https://github.com/goolord/jshark/blob/master/CHANGELOG.md)
  for the detailed pre-release history of the compiler and EDSL.
* Notable surfaces in this release: tagged `Option` with
  `unsafeNullable`/`unsafeOptionToNative` foreign-boundary conversion; the
  `Resolved`-typed `JShark.Promise`; `tryEvaluate`/`EvalFailure` host-eval
  outcomes; `JShark.Internal.validateOptimizedEffect`/`Expr`; and the
  `JShark.Api.Syntax` do-notation convention (`bindExpr`, `toSyntax`,
  `toSyntax_`).
