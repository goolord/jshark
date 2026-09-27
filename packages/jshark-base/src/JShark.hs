-- | One import for JShark: the typed AST, the host evaluator, the compiler,
-- and the EDSL.
--
-- Re-exports "JShark.Core" (AST constructors, 'evaluate', 'pureProgram',
-- 'effectfulProgram', the readable 'pureAST' \/ 'effectfulAST' renderers),
-- "JShark.Api" (the EDSL surface), and the object-literal constructors
-- ('obj', 'frozen', 'field') from "JShark.Object".
--
-- Platform bindings ("JShark.Array", "JShark.Dom", …) stay qualified
-- imports, since they share many names with base and with each other. The
-- IO build driver is "JShark.Build"; "JShark.Prelude" bundles it with the
-- EDSL.
module JShark
  ( module JShark.Core
  , module JShark.Api
  , module JShark.Object
  )
where

import JShark.Api
import JShark.Core
import JShark.Object (field, frozen, obj)
