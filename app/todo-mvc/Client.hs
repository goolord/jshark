{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DeriveGeneric
  , OverloadedRecordDot
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
#-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Client-side TodoMVC written in JShark.
module Client (mainJS, Todo, AppState) where

import Prelude hiding (filter, id)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified JShark.Array as Array
import qualified JShark.Dom as Dom
import qualified JShark.Json as Json
import qualified JShark.Storage as Storage
import qualified JShark.String as String
import Ids
import JShark.Api
import JShark.Generic (MutableObjectOf, newRecord)
import JShark.Object (field, obj)
import JShark.Rec ((<:), Rec(..))
import JShark.Types

-- | Persisted item. JS keys match the selectors (@title@, @completed@, @id@).
data Todo = Todo
  { title :: Text
  , completed :: Bool
  , id :: Int
  }
  deriving (Generic)

-- | Persisted app state. @render@ is a recursive JS binding, not a field.
data AppState = AppState
  { todos :: [Todo]
  , filter :: Text
  , nextId :: Int
  }
  deriving (Generic)

storageKey :: Expr f 'String
storageKey = "jshark-todos"

emptyTodos :: Expr f ('Array (MutableObjectOf Todo))
emptyTodos = Literal (ValueArray [])

parseObject :: Expr f 'String -> Effect f ('MutableObject ())
parseObject = Json.unsafeParse

emptyState :: Effect f (MutableObjectOf AppState)
emptyState = newRecord @AppState

-- | Parse persisted state. 'none' on throw, non-object JSON, or arrays.
-- Missing fields get TodoMVC defaults (@todos=[]@, @nextId=1@, @filter=all@).
parseState :: Expr f 'String -> Effect f ('Option (MutableObjectOf AppState))
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

hydrate :: Expr f ('MutableObject ()) -> EffectSyntax f (Expr f (MutableObjectOf AppState))
hydrate blob = do
  st <- toSyntax emptyState
  t <- getProp (Lift blob) "todos"
  isT <- toSyntax $ ffi "Array.isArray" (arg t <: RecNil)
  ifS (Var isT)
    (set @"todos" st t)
    (set @"todos" st emptyTodos)
  n <- getProp (Lift blob) "nextId"
  fin <- toSyntax $ ffi "Number.isFinite" (arg n <: RecNil)
  ifS (typeOf n .== "number" .&& Var fin)
    (set @"nextId" st n)
    (set @"nextId" st 1)
  f <- getProp (Lift blob) "filter"
  toSyntax $ routeSwitch routeValue (\_ -> set @"filter" st f) f
    (discard (stmts $ set @"filter" st (string valueAll)))
  pure (Var st)

mkTodo :: Expr f 'String -> Expr f 'Number -> EffectSyntax f (Expr f (MutableObjectOf Todo))
mkTodo todoTitle tid = do
  o <- toSyntax $
    obj
      [ field @"title" todoTitle
      , field @"completed" false_
      , field @"id" tid
      ]
  pure (Var o)

hashRecognized :: Expr f 'String -> Expr f 'Bool
hashRecognized hash =
  foldr (\r acc -> hash .== string (routeHash r) .|| acc) false_ routes

showTodo :: Expr f 'String -> Expr f 'Bool -> Expr f 'Bool
showTodo filt isDone =
  if_ (filt .== string valueAll) true_
    (if_ (filt .== string valueActive) (isDone .!= true_) isDone)

routeSwitch
  :: (Route -> Text)
  -> (Route -> EffectSyntax f (f 'Unit))
  -> Expr f 'String
  -> Effect f 'Unit
  -> Effect f 'Unit
routeSwitch key arm scrut def =
  stringCaseE scrut (map (\r -> (key r, discard (stmts (arm r)))) routes) def

applyHashFilter :: Effect f (MutableObjectOf AppState) -> Expr f 'String -> EffectSyntax f (f 'Unit)
applyHashFilter state hash =
  toSyntax $
    routeSwitch routeHash (\r -> set @"filter" state (string (routeValue r))) hash noOp

highlightFilter
  :: Expr f 'String
  -> [(Route, Effect f ('MutableObject Dom.DomElement))]
  -> EffectSyntax f (f 'Unit)
highlightFilter filt filterLinks = do
  mapM_ (\(_, el) -> Dom.classRemove el (string classSelected)) filterLinks
  toSyntax $
    routeSwitch routeValue
      (\r -> maybe done (`Dom.classAdd` string classSelected) (lookup r filterLinks))
      filt
      noOp

persistState
  :: Effect f (MutableObjectOf AppState)
  -> Expr f ('Array (MutableObjectOf Todo))
  -> Expr f 'String
  -> EffectSyntax f (f 'Unit)
persistState state items filt = do
  nid <- state.nextId
  blob <- toSyntax $
    obj
      [ field @"todos" items
      , field @"filter" filt
      , field @"nextId" nid
      ]
      `asTypeOf` emptyState
  Storage.setItem Storage.localStorage storageKey (Json.stringify (Var blob))

byId :: Text -> EffectSyntax f (Effect f ('MutableObject Dom.DomElement))
byId = Dom.lookupId . string

incomplete :: Expr f ('Array (MutableObjectOf Todo)) -> EffectSyntax f (Expr f ('Array (MutableObjectOf Todo)))
incomplete items = Array.filterE_ items $ \t -> do
  c <- t.completed
  toSyntax $ expr (c .!= true_)

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
    whenSomeS (Var parsed) $ \blob -> do
      t <- blob.todos
      set @"todos" state t
      n <- blob.nextId
      set @"nextId" state n
      f <- blob.filter
      set @"filter" state f

  let paint :: Effect f ('Function 'Unit 'Unit) -> EffectSyntax f (f 'Unit)
      paint render = do
        items <- state.todos
        filt <- state.filter

        Dom.setInnerHTML list ""

        forEach_ items $ \todo -> do
          tid <- todo.id
          todoTitle <- todo.title
          isDone <- todo.completed
          whenS (showTodo filt isDone) $ do
            li <- Dom.createElement "li"
            whenS isDone $ Dom.classAdd li "completed"

            view <- Dom.createElement "div"
            Dom.setAttribute view "class" "view"

            checkbox <- Dom.createElement "input"
            Dom.setAttribute checkbox "class" "toggle"
            Dom.setAttribute checkbox "type" "checkbox"
            whenS isDone $ setProp checkbox "checked" true_
            onClick_ checkbox $ do
              cur <- todo.completed
              set @"completed" todo (cur .!= true_)
              call0 render

            label <- Dom.createElement "label"
            Dom.setInnerText label todoTitle

            destroy <- Dom.createElement "button"
            Dom.setAttribute destroy "class" "destroy"
            onClick_ destroy $ do
              items' <- state.todos
              kept <- Array.filterE_ items' $ \t -> do
                i <- t.id
                toSyntax $ expr (i .!= tid)
              set @"todos" state kept
              call0 render

            Dom.appendChild view checkbox
            Dom.appendChild view label
            Dom.appendChild view destroy
            Dom.appendChild li view
            Dom.appendChild list li

        active <- incomplete items
        let activeN = Array.length active
            totalN = Array.length items
            hasTodos = totalN .> 0
        Dom.setInnerText countEl (Show activeN)
        ifS (activeN .== 1)
          (Dom.setInnerText countSuffix " item left")
          (Dom.setInnerText countSuffix " items left")

        ifS hasTodos
          (do
             Dom.setAttribute mainEl "style" ""
             Dom.setAttribute footerEl "style" "")
          (do
             Dom.setAttribute mainEl "style" "display:none"
             Dom.setAttribute footerEl "style" "display:none")

        highlightFilter filt filterLinks
        persistState state items filt

      wire :: Effect f ('Function 'Unit 'Unit) -> EffectSyntax f (f 'Unit)
      wire render = do
        addEventListener_ "submit" form $ do
          inputRaw <- Dom.getValue input
          let todoTitle = String.trim inputRaw
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

  toSyntax $ bindRec
    (\render -> LambdaE $ \_ -> stmts $ paint render)
    (\render -> stmts $ wire render)
