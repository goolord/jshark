# jshark-lucid

Declarative HTML for JShark, using [Lucid](https://hackage.haskell.org/package/lucid)
syntax. HTML definitions compile to imperative `createElement` calls and
event bindings that run in the browser or under a headless DOM.

```haskell
li_ $ do
  classWhen isDone "completed"
  voidWith_ "input" [type_ "checkbox"] $ on "click" toggle
```

See the [TodoMVC example](https://github.com/goolord/jshark/tree/master/examples/src/JShark/Example/TodoMvc)
for a complete application, and the [JShark tutorial](https://github.com/goolord/jshark/blob/master/docs/tutorial.md).

Part of the [JShark](https://github.com/goolord/jshark) monorepo.
