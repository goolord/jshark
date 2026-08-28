{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Client-side TodoMVC written in JShark.
module Client (mainJS, Todo, AppState) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Ids
import JShark.Api
import JShark.Api.Generic (MutableObjectOf, newRecord)
import JShark.Api.Rec (Rec (..), (<:))
import qualified JShark.Array as Array
import qualified JShark.Dom as Dom
import qualified JShark.Json as Json
import JShark.Lucid
  ( JsHtml
  , classWhen
  , dynText
  , on
  , prop
  , renderInto
  , voidWith_
  )
import JShark.Object (field, obj)
import qualified JShark.Storage as Storage
import qualified JShark.String as String
import Lucid (button_, class_, div_, label_, li_, type_)
import Prelude hiding (filter, id)

-- | Persisted item. JS keys match the selectors (@title@, @completed@, @id@).
data Todo = Todo
  { title :: Text
  , completed :: Bool
  , id :: Int
  }
  deriving Generic

-- | Persisted app state. @render@ is a recursive JS binding, not a field.
data AppState = AppState
  { todos :: [Todo]
  , filter :: Text
  , nextId :: Int
  }
  deriving Generic

storageKey :: Expr f 'String
storageKey = "jshark-todos"

emptyTodos :: Expr f ('Array (MutableObjectOf Todo))
emptyTodos = emptyArray

parseObject :: Expr f 'String -> Effect f ('MutableObject ())
parseObject = Json.unsafeParse

emptyState :: Effect f (MutableObjectOf AppState)
emptyState = newRecord @AppState

-- | Parse persisted state. 'none' on throw, non-object JSON, or arrays.
-- Missing fields get TodoMVC defaults (@todos=[]@, @nextId=1@, @filter=all@).
parseState :: Expr f 'String -> Effect f ('Option (MutableObjectOf AppState))
parseState s =
  try_
    ( fromSyntax $ do
        o <- fmap var (toSyntax (parseObject s))
        isArr <- toSyntax $ ffi "Array.isArray" (arg o <: RecNil)
        toSyntax $
          ifE
            (expr (typeOf o .!= "object" .|| var isArr))
            (expr none)
            ( fromSyntax $ do
                st <- hydrate o
                yield (some (var st))
            )
    )
    (expr none)

hydrate ::
  Expr f ('MutableObject ()) -> EffectSyntax f (f (MutableObjectOf AppState))
hydrate blob = do
  st <- toSyntax emptyState
  t <- getProp' blob "todos"
  isT <- toSyntax $ ffi "Array.isArray" (arg t <: RecNil)
  ifS
    (var isT)
    (set @"todos" st t)
    (set @"todos" st emptyTodos)
  n <- getProp' blob "nextId"
  fin <- toSyntax $ ffi "Number.isFinite" (arg n <: RecNil)
  ifS
    (typeOf n .== "number" .&& var fin)
    (set @"nextId" st n)
    (set @"nextId" st 1)
  f <- getProp' blob "filter"
  toSyntax $
    routeSwitch
      routeValue
      (\_ -> set @"filter" st f)
      f
      (discard (stmts $ set @"filter" st (string valueAll)))
  pure st

mkTodo ::
  Expr f 'String
  -> Expr f 'Number
  -> EffectSyntax f (Expr f (MutableObjectOf Todo))
mkTodo todoTitle tid = do
  o <-
    toSyntax $
      obj
        [ field @"title" todoTitle
        , field @"completed" false_
        , field @"id" tid
        ]
  pure (var o)

hashRecognized :: Expr f 'String -> Expr f 'Bool
hashRecognized hash =
  foldr (\r acc -> hash .== string (routeHash r) .|| acc) false_ routes

-- | One todo row, in Lucid's combinators. The structure is TodoMVC's;
-- the holes are the title, the completed class and checkbox state, and the
-- two handlers. 'renderInto' compiles this to @createElement@ calls.
todoItem ::
  Expr f 'String
  -> Expr f 'Bool
  -> EffectSyntax f (f 'Unit)
  -> EffectSyntax f (f 'Unit)
  -> JsHtml f ()
todoItem todoTitle isDone toggle destroy = li_ $ do
  classWhen isDone "completed"
  div_ [class_ "view"] $ do
    voidWith_ "input" [class_ "toggle", type_ "checkbox"] $ do
      prop "checked" isDone
      on "click" toggle
    label_ (dynText todoTitle)
    button_ [class_ "destroy"] (on "click" destroy)

showTodo :: Expr f 'String -> Expr f 'Bool -> Expr f 'Bool
showTodo filt isDone =
  if_
    (filt .== string valueAll)
    true_
    (if_ (filt .== string valueActive) (isDone .!= true_) isDone)

routeSwitch ::
  (Route -> Text)
  -> (Route -> EffectSyntax f (f 'Unit))
  -> Expr f 'String
  -> Effect f 'Unit
  -> Effect f 'Unit
routeSwitch key arm scrut def =
  stringCaseE scrut (map (\r -> (key r, discard (stmts (arm r)))) routes) def

applyHashFilter ::
  Effect f (MutableObjectOf AppState)
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
applyHashFilter state hash =
  toSyntax $
    routeSwitch
      routeHash
      (\r -> set @"filter" state (string (routeValue r)))
      hash
      noOp

highlightFilter ::
  Expr f 'String
  -> [(Route, Effect f ('MutableObject Dom.DomElement))]
  -> EffectSyntax f (f 'Unit)
highlightFilter filt filterLinks = do
  mapM_ (\(_, el) -> Dom.classRemove el (string classSelected)) filterLinks
  toSyntax $
    routeSwitch
      routeValue
      (\r -> maybe done (`Dom.classAdd` string classSelected) (lookup r filterLinks))
      filt
      noOp

persistState ::
  Effect f (MutableObjectOf AppState)
  -> Expr f ('Array (MutableObjectOf Todo))
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
persistState state items filt = do
  nid <- state.nextId
  blob <-
    toSyntax $
      obj
        [ field @"todos" items
        , field @"filter" filt
        , field @"nextId" nid
        ]
        `asTypeOf` emptyState
  Storage.setItem Storage.localStorage storageKey (Json.stringify (var blob))

byId :: Text -> EffectSyntax f (Effect f ('MutableObject Dom.DomElement))
byId = Dom.lookupId . string

incomplete ::
  Expr f ('Array (MutableObjectOf Todo))
  -> EffectSyntax f (Expr f ('Array (MutableObjectOf Todo)))
incomplete items = Array.filterE_ items $ \t -> do
  c <- t.completed
  yield (c .!= true_)

mainJS :: forall f. EffectSyntax f (f 'Unit)
mainJS = do
  form <- byId idForm
  input <- byId idNewTodo
  list <- byId idTodoList
  mainEl <- byId idMain
  footerEl <- byId idFooter
  countEl <- byId idTodoCount
  countSuffix <- byId idTodoCountSuffix
  clearBtn <- byId idClearCompleted
  filterLinks <- traverse (\r -> (,) r <$> byId (routeId r)) routes

  state <- hold emptyState
  set @"todos" state emptyTodos
  set @"filter" state (string valueAll)
  set @"nextId" state 1

  saved <- Storage.getItem Storage.localStorage storageKey
  whenSomeS saved $ \raw -> do
    parsed <- toSyntax $ parseState raw
    whenSomeS (var parsed) $ \blob -> do
      t <- blob.todos
      set @"todos" state t
      n <- blob.nextId
      set @"nextId" state n
      f <- blob.filter
      set @"filter" state f

  let
    paint :: Effect f ('Function 'Unit 'Unit) -> EffectSyntax f (f 'Unit)
    paint render = do
      items <- state.todos
      filt <- state.filter

      Dom.setInnerHTML list ""

      forEach_ items $ \todo -> do
        tid <- todo.id
        todoTitle <- todo.title
        isDone <- todo.completed
        whenS (showTodo filt isDone) $ do
          let
            toggle = do
              cur <- todo.completed
              set @"completed" todo (cur .!= true_)
              call0 render
            destroy = do
              items' <- state.todos
              kept <- Array.filterE_ items' $ \t -> do
                i <- t.id
                yield (i .!= tid)
              set @"todos" state kept
              call0 render
          renderInto list (todoItem todoTitle isDone toggle destroy)

      active <- incomplete items
      let
        activeN = Array.length active
        totalN = Array.length items
        hasTodos = totalN .> 0
      Dom.setInnerText countEl (toString activeN)
      ifS
        (activeN .== 1)
        (Dom.setInnerText countSuffix " item left")
        (Dom.setInnerText countSuffix " items left")

      ifS
        hasTodos
        ( do
            Dom.setAttribute mainEl "style" ""
            Dom.setAttribute footerEl "style" ""
        )
        ( do
            Dom.setAttribute mainEl "style" "display:none"
            Dom.setAttribute footerEl "style" "display:none"
        )

      highlightFilter filt filterLinks
      persistState state items filt

    wire :: Effect f ('Function 'Unit 'Unit) -> EffectSyntax f (f 'Unit)
    wire render = do
      addEventListener_ "submit" form $ do
        inputRaw <- Dom.getValue input
        let
          todoTitle = String.trim inputRaw
        whenS (String.length todoTitle .> 0) $ do
          nid <- state.nextId
          todo <- mkTodo todoTitle nid
          items <- state.todos
          Array.push_ items todo
          set @"nextId" state (nid + 1)
          Dom.setValue input ""
          call0 render

      onClick_ clearBtn $ do
        items <- state.todos
        kept <- incomplete items
        set @"todos" state kept
        call0 render

      -- Hash is the sole filter driver (links are plain <a href="#/...">).
      addEventListener_ "hashchange" window $ do
        hash <- locationHash
        whenS (hashRecognized hash) $ do
          applyHashFilter state hash
          call0 render

      hash0 <- locationHash
      applyHashFilter state hash0

      call0 render

  loop0 paint wire
