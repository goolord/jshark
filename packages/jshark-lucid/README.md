# jshark-lucid

Declarative HTML for JShark, using [Lucid](https://hackage.haskell.org/package/lucid)
syntax. Templates compile to `createElement` calls and event bindings,
ready for the browser or a headless DOM.

```haskell
li_ $ do
  classWhen isDone "completed"
  voidWith_ "input" [type_ "checkbox"] $ on "click" toggle
```

See the [TodoMVC example](https://github.com/goolord/jshark/tree/master/examples/src/JShark/Example/TodoMvc)
for a complete application, or start with the [JShark tutorial](https://github.com/goolord/jshark/blob/master/docs/tutorial.md).

Part of the [JShark](https://github.com/goolord/jshark) monorepo.
