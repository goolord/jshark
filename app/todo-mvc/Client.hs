{-# LANGUAGE
    DataKinds
  , OverloadedStrings
  , ScopedTypeVariables
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
import JShark.Object (unsafeObject)
import JShark.Rec (Rec(..), (<:))
import JShark.Types

storageKey :: Expr f 'String
storageKey = "jshark-todos"

-- | Parse persisted state. 'None' on throw, non-object JSON, or arrays.
-- Missing fields get TodoMVC defaults (@todos=[]@, @nextId=1@, @filter=all@).
-- A true escape (try/catch + JSON.parse), so this is 'ffi' on 'Effect'.
parseStateJS :: String
parseStateJS =
  "(function(s){try{var o=JSON.parse(s);if(!o||typeof o!==\"object\"||Array.isArray(o))return null;return{todos:Array.isArray(o.todos)?o.todos:[],nextId:(typeof o.nextId===\"number\"&&isFinite(o.nextId))?o.nextId:1,filter:(o.filter===\"active\"||o.filter===\"completed\")?o.filter:\"all\"};}catch(e){return null;}})"

callRender :: Effect f ('Object ()) -> EffectSyntax f (f 'Unit)
callRender state = do
  r <- getProp state "render"
  call0 r

mkTodo :: Expr f 'String -> Expr f 'Number -> EffectSyntax f (Expr f ('Object ()))
mkTodo title tid = do
  o <- toSyntax emptyObject
  setProp (obj o) "title" title
  setProp (obj o) "completed" false_
  setProp (obj o) "id" tid
  pure (toExpr o)

hashRecognized :: Expr f 'String -> Expr f 'Bool
hashRecognized hash =
  hash .== "#/active" .|| hash .== "#/completed" .|| hash .== "#/"

applyHashFilter :: Effect f ('Object ()) -> Expr f 'String -> EffectSyntax f (f 'Unit)
applyHashFilter state hash =
  ifS (hash .== "#/active")
    (setProp state "filter" "active")
    (ifS (hash .== "#/completed")
       (setProp state "filter" "completed")
       (ifS (hash .== "#/")
          (setProp state "filter" "all")
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

  state <- hold (unsafeObject "{todos:[],filter:\"all\",nextId:1}" :: Effect f ('Object ()))

  saved <- Storage.getItem Storage.localStorage storageKey
  whenSomeS saved $ \raw -> do
    parsed <- toSyntax $ ffi parseStateJS (arg raw <: RecNil)
    whenSomeS (unsafeNullable (Var parsed) :: Expr f ('Option ('Object ()))) $ \blob -> do
      t <- getProp' blob "todos"
      setProp state "todos" t
      n <- getProp' blob "nextId"
      setProp state "nextId" n
      f <- getProp' blob "filter"
      setProp state "filter" f

  render <- toSyntax $ LambdaE $ \_ -> stmts $ do
    todos <- getProp state "todos" :: EffectSyntax f (Expr f ('Array ('Object ())))
    filt <- getProp state "filter"

    Dom.setInnerHTML list ""

    forEach_ todos $ \todo -> do
      tid <- getProp' todo "id"
      title <- getProp' todo "title"
      completed <- getProp' todo "completed"
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
          cur <- getProp' todo "completed"
          setProp' todo "completed" (cur .!= true_)
          callRender state

        label <- Dom.createElement "label"
        Dom.setInnerText label title

        destroy <- Dom.createElement "button"
        Dom.setAttribute destroy "class" "destroy"
        onClick_ destroy $ do
          todos' <- getProp state "todos" :: EffectSyntax f (Expr f ('Array ('Object ())))
          kept <- Array.filterE_ todos' $ \t -> do
            i <- getProp' t "id"
            toSyntax $ expr (i .!= tid)
          setProp state "todos" kept
          callRender state

        Dom.appendChild view checkbox
        Dom.appendChild view label
        Dom.appendChild view destroy
        Dom.appendChild li view
        Dom.appendChild list li

    active <- Array.filterE_ todos $ \t -> do
      c <- getProp' t "completed"
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

    blob <- toSyntax emptyObject
    setProp' blob "todos" todos
    nid <- getProp state "nextId"
    setProp' blob "nextId" nid
    setProp' blob "filter" filt
    Storage.setItem Storage.localStorage storageKey (Json.stringify (Var blob))

  setProp state "render" (Var render)

  addEventListener_ "submit" form $ do
    inputRaw <- Dom.getValue input
    let title = String.trim inputRaw
    whenS (String.length_ title .> 0) $ do
      nid <- getProp state "nextId"
      todo <- mkTodo title nid
      todos <- getProp state "todos"
      Array.push_ todos todo
      setProp state "nextId" (nid + 1)
      Dom.setValue input ""
      callRender state

  onClick_ clearBtn $ do
    todos <- getProp state "todos" :: EffectSyntax f (Expr f ('Array ('Object ())))
    kept <- Array.filterE_ todos $ \t -> do
      c <- getProp' t "completed"
      toSyntax $ expr (c .!= true_)
    setProp state "todos" kept
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
