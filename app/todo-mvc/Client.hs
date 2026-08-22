{-# LANGUAGE
    DataKinds
  , OverloadedStrings
  , ScopedTypeVariables
  ,     TypeApplications
    , TypeFamilies
    , AllowAmbiguousTypes
#-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Client-side TodoMVC written in JShark.
module Client (mainJS) where

import qualified JShark.Array as Array
import qualified JShark.Dom as Dom
import qualified JShark.Json as Json
import qualified JShark.Storage as Storage
import qualified JShark.String as String
import JShark.Api
import JShark.Rec ((<:), Rec(..))
import JShark.Types

data Todo
data AppState

type instance Field Todo "title" = 'String
type instance Field Todo "completed" = 'Bool
type instance Field Todo "id" = 'Number

type instance Field AppState "todos" = 'Array ('Object Todo)
type instance Field AppState "filter" = 'String
type instance Field AppState "nextId" = 'Number
type instance Field AppState "render" = 'Function 'Unit 'Unit

storageKey :: Expr f 'String
storageKey = "jshark-todos"

emptyTodos :: Expr f ('Array ('Object Todo))
emptyTodos = Literal (ValueArray [])

parseObject :: Expr f 'String -> Effect f ('Object ())
parseObject = Json.unsafeParse

emptyState :: Effect f ('Object AppState)
emptyState = newObject

emptyTodo :: Effect f ('Object Todo)
emptyTodo = newObject

-- | Parse persisted state. 'none' on throw, non-object JSON, or arrays.
-- Missing fields get TodoMVC defaults (@todos=[]@, @nextId=1@, @filter=all@).
parseState :: Expr f 'String -> Effect f ('Option ('Object AppState))
parseState s = try_
  (fromSyntax $ do
    o <- toSyntax (parseObject s)
    isArr <- toSyntax $ ffi "Array.isArray" (arg (Var o) <: RecNil)
    toSyntax $ ifE (expr (typeOf (Var o) .!= "object" .|| Var isArr))
      (expr none)
      (fromSyntax $ do
        st <- hydrate (Var o)
        yield (some st)))
  (expr none)

hydrate :: Expr f ('Object ()) -> EffectSyntax f (Expr f ('Object AppState))
hydrate blob = do
  st <- toSyntax emptyState
  t <- getProp (Lift blob) "todos"
  isT <- toSyntax $ ffi "Array.isArray" (arg t <: RecNil)
  ifS (Var isT)
    (set @"todos" (Lift (Var st)) t)
    (set @"todos" (Lift (Var st)) emptyTodos)
  n <- getProp (Lift blob) "nextId"
  fin <- toSyntax $ ffi "Number.isFinite" (arg n <: RecNil)
  ifS (typeOf n .== "number" .&& Var fin)
    (set @"nextId" (Lift (Var st)) n)
    (set @"nextId" (Lift (Var st)) 1)
  f <- getProp (Lift blob) "filter"
  ifS (f .== "active" .|| f .== "completed")
    (set @"filter" (Lift (Var st)) f)
    (set @"filter" (Lift (Var st)) "all")
  pure (Var st)

callRender :: Effect f ('Object AppState) -> EffectSyntax f (f 'Unit)
callRender state = do
  r <- get @"render" state
  call0 r

mkTodo :: Expr f 'String -> Expr f 'Number -> EffectSyntax f (Expr f ('Object Todo))
mkTodo title tid = do
  o <- toSyntax emptyTodo
  set @"title" (Lift (Var o)) title
  set @"completed" (Lift (Var o)) false_
  set @"id" (Lift (Var o)) tid
  pure (Var o)

hashRecognized :: Expr f 'String -> Expr f 'Bool
hashRecognized hash =
  hash .== "#/active" .|| hash .== "#/completed" .|| hash .== "#/"

applyHashFilter :: Effect f ('Object AppState) -> Expr f 'String -> EffectSyntax f (f 'Unit)
applyHashFilter state hash =
  ifS (hash .== "#/active")
    (set @"filter" state "active")
    (ifS (hash .== "#/completed")
       (set @"filter" state "completed")
       (ifS (hash .== "#/")
          (set @"filter" state "all")
          done))

mainJS :: forall f. EffectSyntax f (f 'Unit)
mainJS = do
  form <- Dom.lookupId "todo-form"
  input <- Dom.lookupId "new-todo"
  list <- Dom.lookupId "todo-list"
  mainEl <- Dom.lookupId "main"
  footer <- Dom.lookupId "footer"
  countEl <- Dom.lookupId "todo-count"
  countSuffix <- Dom.lookupId "todo-count-suffix"
  clearBtn <- Dom.lookupId "clear-completed"
  filterAll <- Dom.lookupId "filter-all"
  filterActive <- Dom.lookupId "filter-active"
  filterCompleted <- Dom.lookupId "filter-completed"

  state <- hold emptyState
  set @"todos" state emptyTodos
  set @"filter" state "all"
  set @"nextId" state 1

  saved <- Storage.getItem Storage.localStorage storageKey
  whenSomeS saved $ \raw -> do
    parsed <- toSyntax $ parseState raw
    whenSomeS (Var parsed) $ \blob -> do
      t <- get @"todos" (expr blob)
      set @"todos" state t
      n <- get @"nextId" (expr blob)
      set @"nextId" state n
      f <- get @"filter" (expr blob)
      set @"filter" state f

  render <- toSyntax $ LambdaE $ \_ -> stmts $ do
    todos <- get @"todos" state
    filt <- get @"filter" state

    Dom.setInnerHTML list ""

    forEach_ todos $ \todo -> do
      tid <- get @"id" (expr todo)
      title <- get @"title" (expr todo)
      completed <- get @"completed" (expr todo)
      let showTodo =
            if_ (filt .== "all") true_
              (if_ (filt .== "active") (completed .!= true_) completed)
      whenS showTodo $ do
        li <- Dom.createElement "li"
        whenS completed $ Dom.classAdd li "completed"

        view <- Dom.createElement "div"
        Dom.setAttribute view "class" "view"

        checkbox <- Dom.createElement "input"
        Dom.setAttribute checkbox "class" "toggle"
        Dom.setAttribute checkbox "type" "checkbox"
        whenS completed $ setProp checkbox "checked" true_
        onClick_ checkbox $ do
          cur <- get @"completed" (expr todo)
          set @"completed" (expr todo) (cur .!= true_)
          callRender state

        label <- Dom.createElement "label"
        Dom.setInnerText label title

        destroy <- Dom.createElement "button"
        Dom.setAttribute destroy "class" "destroy"
        onClick_ destroy $ do
          todos' <- get @"todos" state
          kept <- Array.filterE_ todos' $ \t -> do
            i <- get @"id" (expr t)
            toSyntax $ expr (i .!= tid)
          set @"todos" state kept
          callRender state

        Dom.appendChild view checkbox
        Dom.appendChild view label
        Dom.appendChild view destroy
        Dom.appendChild li view
        Dom.appendChild list li

    active <- Array.filterE_ todos $ \t -> do
      c <- get @"completed" (expr t)
      toSyntax $ expr (c .!= true_)
    let activeN = Array.length_ active
        totalN = Array.length_ todos
        hasTodos = totalN .> 0
    Dom.setInnerText countEl (Show activeN)
    ifS (activeN .== 1)
      (Dom.setInnerText countSuffix " item left")
      (Dom.setInnerText countSuffix " items left")

    ifS hasTodos
      (do
         Dom.setAttribute mainEl "style" ""
         Dom.setAttribute footer "style" "")
      (do
         Dom.setAttribute mainEl "style" "display:none"
         Dom.setAttribute footer "style" "display:none")

    Dom.classRemove filterAll "selected"
    Dom.classRemove filterActive "selected"
    Dom.classRemove filterCompleted "selected"
    ifS (filt .== "active")
      (Dom.classAdd filterActive "selected")
      (ifS (filt .== "completed")
         (Dom.classAdd filterCompleted "selected")
         (Dom.classAdd filterAll "selected"))

    blob <- toSyntax emptyState
    set @"todos" (Lift (Var blob)) todos
    nid <- get @"nextId" state
    set @"nextId" (Lift (Var blob)) nid
    set @"filter" (Lift (Var blob)) filt
    Storage.setItem Storage.localStorage storageKey (Json.stringify (Var blob))

  set @"render" state (Var render)

  addEventListener_ "submit" form $ do
    inputRaw <- Dom.getValue input
    let title = String.trim inputRaw
    whenS (String.length_ title .> 0) $ do
      nid <- get @"nextId" state
      todo <- mkTodo title nid
      todos <- get @"todos" state
      Array.push_ todos todo
      set @"nextId" state (nid + 1)
      Dom.setValue input ""
      callRender state

  onClick_ clearBtn $ do
    todos <- get @"todos" state
    kept <- Array.filterE_ todos $ \t -> do
      c <- get @"completed" (expr t)
      toSyntax $ expr (c .!= true_)
    set @"todos" state kept
    callRender state

  -- Hash is the sole filter driver (links are plain <a href="#/...">).
  addEventListener_ "hashchange" window $ do
    hash <- locationHash
    whenS (hashRecognized hash) $ do
      applyHashFilter state hash
      callRender state

  hash0 <- locationHash
  applyHashFilter state hash0

  callRender state
