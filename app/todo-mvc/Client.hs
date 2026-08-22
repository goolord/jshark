{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , OverloadedStrings
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
#-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Client-side TodoMVC written in JShark.
module Client (mainJS) where

import Data.Text (Text)
import qualified JShark.Array as Array
import qualified JShark.Dom as Dom
import qualified JShark.Json as Json
import qualified JShark.Storage as Storage
import qualified JShark.String as String
import Ids
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

parseObject :: Expr f 'String -> Effect f ('Object AppState)
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

hydrate :: Expr f ('Object AppState) -> EffectSyntax f (Expr f ('Object AppState))
hydrate blob = do
  st <- toSyntax emptyState
  t <- get @"todos" blob
  isT <- toSyntax $ ffi "Array.isArray" (arg t <: RecNil)
  ifS (Var isT)
    (set @"todos" st t)
    (set @"todos" st emptyTodos)
  n <- get @"nextId" blob
  fin <- toSyntax $ ffi "Number.isFinite" (arg n <: RecNil)
  ifS (typeOf n .== "number" .&& Var fin)
    (set @"nextId" st n)
    (set @"nextId" st 1)
  f <- get @"filter" blob
  ifS (knownFilter f)
    (set @"filter" st f)
    (set @"filter" st (string valueAll))
  pure (Var st)

knownFilter :: Expr f 'String -> Expr f 'Bool
knownFilter f = foldr (\r acc -> f .== string (routeValue r) .|| acc) false_ routes

callRender :: Effect f ('Object AppState) -> EffectSyntax f (f 'Unit)
callRender state = do
  r <- get @"render" state
  call0 r

mkTodo :: Expr f 'String -> Expr f 'Number -> EffectSyntax f (Expr f ('Object Todo))
mkTodo title tid = do
  o <- toSyntax emptyTodo
  set @"title" o title
  set @"completed" o false_
  set @"id" o tid
  pure (Var o)

hashRecognized :: Expr f 'String -> Expr f 'Bool
hashRecognized hash =
  foldr (\r acc -> hash .== string (routeHash r) .|| acc) false_ routes

applyHashFilter :: Effect f ('Object AppState) -> Expr f 'String -> EffectSyntax f (f 'Unit)
applyHashFilter state hash =
  foldr
    (\r k ->
       ifS (hash .== string (routeHash r))
         (set @"filter" state (string (routeValue r)))
         k)
    done
    routes

byId :: Text -> EffectSyntax f (Effect f ('Object Dom.DomElement))
byId = Dom.lookupId . string

incomplete :: Expr f ('Array ('Object Todo)) -> EffectSyntax f (Expr f ('Array ('Object Todo)))
incomplete todos = Array.filterE_ todos $ \t -> do
  c <- get @"completed" t
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
      t <- get @"todos" blob
      set @"todos" state t
      n <- get @"nextId" blob
      set @"nextId" state n
      f <- get @"filter" blob
      set @"filter" state f

  render <- toSyntax $ LambdaE $ \_ -> stmts $ do
    todos <- get @"todos" state
    filt <- get @"filter" state

    Dom.setInnerHTML list ""

    forEach_ todos $ \todo -> do
      tid <- get @"id" todo
      title <- get @"title" todo
      completed <- get @"completed" todo
      let showTodo =
            if_ (filt .== string valueAll) true_
              (if_ (filt .== string valueActive) (completed .!= true_) completed)
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
          cur <- get @"completed" todo
          set @"completed" todo (cur .!= true_)
          callRender state

        label <- Dom.createElement "label"
        Dom.setInnerText label title

        destroy <- Dom.createElement "button"
        Dom.setAttribute destroy "class" "destroy"
        onClick_ destroy $ do
          todos' <- get @"todos" state
          kept <- Array.filterE_ todos' $ \t -> do
            i <- get @"id" t
            toSyntax $ expr (i .!= tid)
          set @"todos" state kept
          callRender state

        Dom.appendChild view checkbox
        Dom.appendChild view label
        Dom.appendChild view destroy
        Dom.appendChild li view
        Dom.appendChild list li

    active <- incomplete todos
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
         Dom.setAttribute footerEl "style" "")
      (do
         Dom.setAttribute mainEl "style" "display:none"
         Dom.setAttribute footerEl "style" "display:none")

    mapM_ (\(_, el) -> Dom.classRemove el (string classSelected)) filterLinks
    mapM_ (\(r, el) ->
      ifS (filt .== string (routeValue r))
        (Dom.classAdd el (string classSelected))
        done) filterLinks

    blob <- toSyntax emptyState
    set @"todos" blob todos
    nid <- get @"nextId" state
    set @"nextId" blob nid
    set @"filter" blob filt
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
    kept <- incomplete todos
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
