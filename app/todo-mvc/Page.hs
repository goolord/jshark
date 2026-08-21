{-# LANGUAGE
    OverloadedStrings
#-}

module Page (page) where

import Lucid

-- | TodoMVC shell. Client script is loaded from @/app.js@.
page :: Html ()
page = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "JShark • TodoMVC"
    link_
      [ rel_ "stylesheet"
      , href_ "https://unpkg.com/todomvc-common@1.0.5/base.css"
      ]
    link_
      [ rel_ "stylesheet"
      , href_ "https://unpkg.com/todomvc-app-css@2.4.3/index.css"
      ]
  body_ $ do
    section_ [class_ "todoapp"] $ do
      header_ [class_ "header"] $ do
        h1_ "todos"
        form_ [id_ "todo-form", onsubmit_ "return false;"] $
          input_
            [ class_ "new-todo"
            , id_ "new-todo"
            , placeholder_ "What needs to be done?"
            , autofocus_
            ]
      section_ [class_ "main", id_ "main"] $
        ul_ [class_ "todo-list", id_ "todo-list"] mempty
      footer_ [class_ "footer", id_ "footer"] $ do
        span_ [class_ "todo-count"] $ do
          strong_ [id_ "todo-count"] "0"
          span_ [id_ "todo-count-suffix"] " items left"
        ul_ [class_ "filters"] $ do
          li_ $ a_ [href_ "#/", id_ "filter-all", class_ "selected"] "All"
          li_ $ a_ [href_ "#/active", id_ "filter-active"] "Active"
          li_ $ a_ [href_ "#/completed", id_ "filter-completed"] "Completed"
        button_ [class_ "clear-completed", id_ "clear-completed"] "Clear completed"
    footer_ [class_ "info"] $ do
      p_ "Enter to add a todo"
      p_ $ do
        "Created with "
        a_ [href_ "https://github.com/goolord/jshark"] "JShark"
        ", Scotty, and Lucid"
    script_ [src_ "/app.js"] ("" :: Html ())
