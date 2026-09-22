# Changelog

## 0.1.0.0

* First release. See the [repository changelog](https://github.com/goolord/jshark/blob/master/CHANGELOG.md)
  for the full history of this package and the rest of the monorepo.
* Snapshot and subscription are taken in one STM transaction, and each
  publication bumps a monotonic revision carried on the snapshot. The SSE
  loop polls so a disconnected client is torn down promptly. HTML rewrites
  drop stale `ETag`/`Content-MD5`, and the watcher drain worker is joined
  on dispose so start/stop cycles do not leak threads.
* The embedded browser runtime is decoded as UTF-8 explicitly, so the
  package builds under a non-UTF-8 locale.
* `JShark.HotReload.Core` reads hub state through one `currentSnapshot`
  (replacing `currentJsHashes`, `currentRevision`, `lastBuildError`, and
  `lastCompiling`); `setBuildError` / `setBuildStart` are dropped in favour
  of `broadcastEvent` with `BuildError` / `BuildStart`.
