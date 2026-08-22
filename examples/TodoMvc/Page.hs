{-# LANGUAGE OverloadedStrings #-}

module Page (page) where

import qualified Data.Text as T
import Ids
import Lucid

-- | TodoMVC shell. @headExtra@ / @source@ are the highlighter and JS pane.
page :: Html () -> Html () -> T.Text -> Html ()
page headExtra source scriptSrc = doctypehtml_ $ do
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
    style_
      ( "body{max-width:none}"
          <> ".todoapp,.info{max-width:550px;margin-left:auto;margin-right:auto}"
      )
    headExtra
  body_ $ do
    section_ [class_ "todoapp"] $ do
      header_ [class_ "header"] $ do
        h1_ "todos"
        form_ [id_ idForm, onsubmit_ "return false;"] $
          input_
            [ class_ "new-todo"
            , id_ idNewTodo
            , placeholder_ "What needs to be done?"
            , autofocus_
            ]
      section_ [class_ "main", id_ idMain] $
        ul_ [class_ "todo-list", id_ idTodoList] mempty
      footer_ [class_ "footer", id_ idFooter] $ do
        span_ [class_ "todo-count"] $ do
          strong_ [id_ idTodoCount] "0"
          span_ [id_ idTodoCountSuffix] " items left"
        ul_ [class_ "filters"] $ mapM_ filterLink routes
        button_ [class_ "clear-completed", id_ idClearCompleted] "Clear completed"
    footer_ [class_ "info"] $ do
      p_ "Enter to add a todo"
      p_ $ do
        "Created with "
        a_ [href_ "https://github.com/goolord/jshark"] "JShark"
        ", Scotty, and Lucid"
    source
    script_ [src_ scriptSrc] ("" :: Html ())

filterLink :: Route -> Html ()
filterLink r =
  li_ $ a_ attrs (toHtml (routeLabel r))
 where
  attrs =
    href_ (routeHash r)
      : id_ (routeId r)
      : if routeValue r == valueAll then [class_ classSelected] else []
