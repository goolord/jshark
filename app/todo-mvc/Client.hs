{-# LANGUAGE
    DataKinds
  , OverloadedStrings
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
import JShark.Object (unsafeObject, unsafeObjectAssign, unsafeObjectGet)
import JShark.Rec (Rec(..), (<:))
import JShark.Types

storageKey :: Expr f 'String
storageKey = string "jshark-todos"

getValue :: Effect f ('Object Dom.DomElement) -> EffectSyntax f (Expr f 'String)
getValue el = fmap Var $ toSyntax $ unsafeObjectGet el "value"

setValue :: Effect f ('Object Dom.DomElement) -> Expr f 'String -> EffectSyntax f ()
setValue el v = toSyntax_ $ unsafeObjectAssign (unsafeObjectGet el "value") (Lift v)

getProp :: Effect f ('Object a) -> String -> EffectSyntax f (Expr f u)
getProp obj name = fmap Var $ toSyntax $ unsafeObjectGet obj name

setProp :: Effect f ('Object a) -> String -> Expr f u -> EffectSyntax f ()
setProp obj name v = toSyntax_ $ unsafeObjectAssign (unsafeObjectGet obj name) (Lift v)

-- | Parse persisted state. 'None' on throw, non-object JSON, or arrays.
-- Missing fields get TodoMVC defaults (@todos=[]@, @nextId=1@, @filter=all@).
tryParseState :: Expr f 'String -> Expr f ('Option ('Object ()))
tryParseState s =
  unsafeNullable $
    exprFfi
      "(function(s){try{var o=JSON.parse(s);if(!o||typeof o!==\"object\"||Array.isArray(o))return null;return{todos:Array.isArray(o.todos)?o.todos:[],nextId:(typeof o.nextId===\"number\"&&isFinite(o.nextId))?o.nextId:1,filter:(o.filter===\"active\"||o.filter===\"completed\")?o.filter:\"all\"};}catch(e){return null;}})"
      (s <: RecNil)

-- | Invoke @state.render()@. Stored on the state object so the render
-- body can call itself without a recursive Haskell binding.
callRender :: Effect f ('Object a) -> EffectSyntax f ()
callRender state = do
  r <- getProp state "render"
  toSyntax_ $ ApplyE (Lift r) noOp

mkTodo :: Expr f 'String -> Expr f 'Number -> EffectSyntax f (Expr f ('Object ()))
mkTodo title tid = do
  o <- toSyntax $ unsafeObject "{}"
  setProp (Lift (Var o)) "title" title
  setProp (Lift (Var o)) "completed" (bool False)
  setProp (Lift (Var o)) "id" tid
  pure (Var o)

hashRecognized :: Expr f 'String -> Expr f 'Bool
hashRecognized hash =
  Or (Eq hash (string "#/active"))
     (Or (Eq hash (string "#/completed")) (Eq hash (string "#/")))

applyHashFilter :: Effect f ('Object a) -> Expr f 'String -> Effect f 'Unit
applyHashFilter state hash =
  ifE (Eq hash (string "#/active"))
    (fromSyntax $ setProp state "filter" (string "active") *> toSyntax noOp)
    (ifE (Eq hash (string "#/completed"))
       (fromSyntax $ setProp state "filter" (string "completed") *> toSyntax noOp)
       (ifE (Eq hash (string "#/"))
          (fromSyntax $ setProp state "filter" (string "all") *> toSyntax noOp)
          noOp))

mainJS :: EffectSyntax f (f 'Unit)
mainJS = do
  form <- Dom.lookupId (string "todo-form")
  input <- Dom.lookupId (string "new-todo")
  list <- Dom.lookupId (string "todo-list")
  mainEl <- Dom.lookupId (string "main")
  footer <- Dom.lookupId (string "footer")
  countEl <- Dom.lookupId (string "todo-count")
  countSuffix <- Dom.lookupId (string "todo-count-suffix")
  clearBtn <- Dom.lookupId (string "clear-completed")
  filterAll <- Dom.lookupId (string "filter-all")
  filterActive <- Dom.lookupId (string "filter-active")
  filterCompleted <- Dom.lookupId (string "filter-completed")

  state <- fmap (expr . Var) $ toSyntax $ unsafeObject "{todos:[],filter:\"all\",nextId:1}"

  saved <- Storage.getItem Storage.localStorage storageKey
  let optBlob = optionCase saved none tryParseState
      hasBlob = optionCase optBlob (bool False) (\_ -> bool True)
  toSyntax_ $ when_ hasBlob $ fromSyntax $ do
    blob <- fmap (expr . Var) $ toSyntax $ Lift $
      optionCase optBlob (exprFfi "Object" RecNil) id
    t <- getProp blob "todos"
    setProp state "todos" t
    n <- getProp blob "nextId"
    setProp state "nextId" n
    f <- getProp blob "filter"
    setProp state "filter" f
    toSyntax noOp

  render <- toSyntax $ LambdaE $ \_ -> fromSyntax $ do
    todos <- getProp state "todos"
    filt <- getProp state "filter"

    Dom.setInnerHTML list (string "")

    toSyntax_ $ forEach todos $ \todo -> fromSyntax $ do
      let tid = exprProp todo "id"
          title = exprProp todo "title"
          completed = exprProp todo "completed"
          showTodo =
            If
              (Eq filt (string "all"))
              (bool True)
              (If (Eq filt (string "active")) (NEq completed (bool True)) completed)
      toSyntax_ $ when_ showTodo $ fromSyntax $ do
        li <- Dom.createElement (string "li")
        toSyntax_ $ when_ completed $ fromSyntax $ do
          Dom.classAdd li (string "completed")
          toSyntax noOp

        view <- Dom.createElement (string "div")
        Dom.setAttribute view "class" (string "view")

        checkbox <- Dom.createElement (string "input")
        Dom.setAttribute checkbox "class" (string "toggle")
        Dom.setAttribute checkbox "type" (string "checkbox")
        toSyntax_ $ when_ completed $ fromSyntax $ do
          setProp checkbox "checked" (bool True)
          toSyntax noOp
        onClick checkbox $ \_ -> fromSyntax $ do
          cur <- fmap Var $ toSyntax $ unsafeObjectGet (Lift todo) "completed"
          setProp (Lift todo) "completed" (NEq cur (bool True))
          callRender state
          toSyntax noOp

        label <- Dom.createElement (string "label")
        Dom.setInnerText label title

        destroy <- Dom.createElement (string "button")
        Dom.setAttribute destroy "class" (string "destroy")
        onClick destroy $ \_ -> fromSyntax $ do
          todos' <- getProp state "todos"
          setProp state "todos" $
            Array.filter_ todos' $ \t -> NEq (exprProp t "id") tid
          callRender state
          toSyntax noOp

        Dom.appendChild view checkbox
        Dom.appendChild view label
        Dom.appendChild view destroy
        Dom.appendChild li view
        Dom.appendChild list li
        toSyntax noOp
      toSyntax noOp

    let active = Array.filter_ todos $ \t -> NEq (exprProp t "completed") (bool True)
        activeN = Array.length_ active
        totalN = Array.length_ todos
        hasTodos = GTh totalN (number 0)
    Dom.setInnerText countEl (Show activeN)
    toSyntax_ $ ifE (Eq activeN (number 1))
      (fromSyntax $ Dom.setInnerText countSuffix (string " item left") *> toSyntax noOp)
      (fromSyntax $ Dom.setInnerText countSuffix (string " items left") *> toSyntax noOp)

    toSyntax_ $ ifE hasTodos
      (fromSyntax $ do
         Dom.setAttribute mainEl "style" (string "")
         Dom.setAttribute footer "style" (string "")
         toSyntax noOp)
      (fromSyntax $ do
         Dom.setAttribute mainEl "style" (string "display:none")
         Dom.setAttribute footer "style" (string "display:none")
         toSyntax noOp)

    Dom.classRemove filterAll (string "selected")
    Dom.classRemove filterActive (string "selected")
    Dom.classRemove filterCompleted (string "selected")
    toSyntax_ $ ifE (Eq filt (string "active"))
      (fromSyntax $ Dom.classAdd filterActive (string "selected") *> toSyntax noOp)
      (ifE (Eq filt (string "completed"))
         (fromSyntax $ Dom.classAdd filterCompleted (string "selected") *> toSyntax noOp)
         (fromSyntax $ Dom.classAdd filterAll (string "selected") *> toSyntax noOp))

    blob <- toSyntax $ unsafeObject "{}"
    setProp (Lift (Var blob)) "todos" todos
    nid <- getProp state "nextId"
    setProp (Lift (Var blob)) "nextId" nid
    setProp (Lift (Var blob)) "filter" filt
    Storage.setItem Storage.localStorage storageKey (Json.stringify (Var blob))
    toSyntax noOp

  setProp state "render" (Var render)

  addEventListener "submit" form $ \_ -> fromSyntax $ do
    inputRaw <- getValue input
    let title = String.trim inputRaw
    toSyntax_ $ when_ (GTh (String.length_ title) (number 0)) $ fromSyntax $ do
      nid <- getProp state "nextId"
      todo <- mkTodo title nid
      todos <- getProp state "todos"
      toSyntax_ $ Array.push todos todo
      setProp state "nextId" (plus nid (number 1))
      setValue input (string "")
      callRender state
      toSyntax noOp
    toSyntax noOp

  onClick clearBtn $ \_ -> fromSyntax $ do
    todos <- getProp state "todos"
    setProp state "todos" $
      Array.filter_ todos $ \t -> NEq (exprProp t "completed") (bool True)
    callRender state
    toSyntax noOp

  -- Hash is the sole filter driver (links are plain <a href="#/...">).
  toSyntax_ $ ffi "window.addEventListener"
    ( string "hashchange"
        <: unsafeEffectExpr
          ( LambdaE $ \_ -> fromSyntax $ do
              hash <- fmap Var $ toSyntax $ unsafeObjectGet (unsafeObject "location") "hash"
              toSyntax_ $ when_ (hashRecognized hash) $ fromSyntax $ do
                toSyntax_ $ applyHashFilter state hash
                callRender state
                toSyntax noOp
              toSyntax noOp
          )
        <: RecNil
    )

  hash0 <- fmap Var $ toSyntax $ unsafeObjectGet (unsafeObject "location") "hash"
  toSyntax_ $ applyHashFilter state hash0

  callRender state
  toSyntax noOp
