{-# LANGUAGE OverloadedStrings #-}

module Page (page) where

import qualified Data.Text as T
import Ids
import Lucid

-- | TodoMVC shell. @staticRoot@ is the shared assets prefix; @headExtra@ /
-- @source@ are the highlighter and JS pane.
page :: T.Text -> Html () -> Html () -> T.Text -> Html ()
page staticRoot headExtra source scriptSrc = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "Todos"
    link_ [rel_ "stylesheet", href_ (staticRoot <> "/todomvc-common-base.css")]
    link_ [rel_ "stylesheet", href_ (staticRoot <> "/todomvc-app-index.css")]
    link_ [rel_ "stylesheet", href_ (staticRoot <> "/todo-mvc.css")]
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
    footer_ [class_ "info"] $
      p_ "Enter to add · filters in footer"
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
