{-# LANGUAGE OverloadedStrings #-}

module JShark.Example.TodoMvc.Page (page) where

import JShark.Example.Theme (ExamplePage, examplePage)
import JShark.Example.TodoMvc.Ids
import Lucid

-- | TodoMVC shell.
page :: ExamplePage
page = examplePage id "Todos" sheets $ do
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
  footer_ [class_ "info"] $ p_ "Enter to add · filters in footer"
 where
  sheets =
    [ "/css/todomvc-common-base.css"
    , "/css/todomvc-app-index.css"
    , "/css/todo-mvc.css"
    ]

filterLink :: Route -> Html ()
filterLink r = li_ $ a_ (href_ (routeHash r) : id_ (routeId r) : selected) label
 where
  label = toHtml (routeLabel r)
  selected = [class_ classSelected | routeValue r == valueAll]
