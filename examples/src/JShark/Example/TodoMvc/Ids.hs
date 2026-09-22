{-# LANGUAGE OverloadedStrings #-}

-- | DOM ids and the hash/filter strings they imply. Page and Client
-- must use these; do not restate the literals in either file.
module JShark.Example.TodoMvc.Ids (module JShark.Example.TodoMvc.Ids) where

import Data.Text (Text)

data Route = Route {routeId, routeHash, routeValue, routeLabel :: Text}
  deriving Eq

routeAll, routeActive, routeCompleted :: Route
routeAll = Route "filter-all" "#/" "all" "All"
routeActive = Route "filter-active" "#/active" "active" "Active"
routeCompleted = Route "filter-completed" "#/completed" "completed" "Completed"

routes :: [Route]
routes = [routeAll, routeActive, routeCompleted]

valueAll, valueActive :: Text
valueAll = routeValue routeAll
valueActive = routeValue routeActive

classSelected, idForm, idNewTodo, idTodoList, idMain, idFooter :: Text
classSelected = "selected"
idForm = "todo-form"
idNewTodo = "new-todo"
idTodoList = "todo-list"
idMain = "main"
idFooter = "footer"

idTodoCount, idTodoCountSuffix, idClearCompleted :: Text
idTodoCount = "todo-count"
idTodoCountSuffix = "todo-count-suffix"
idClearCompleted = "clear-completed"
