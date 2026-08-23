{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Closed-name 'RegExp'. Emitted as @new RegExp("…")@, never a @/re/@ literal.
module JShark.Regex
  ( regex
  , test
  )
where

import Data.Text (Text)
import JShark.Types

-- | @new RegExp(source)@. The source is a Haskell 'Text' constant, not
-- spliced into a regex literal.
regex :: Text -> Expr f 'Regex
regex = Literal . ValueRegex

-- | @re.test(s)@
test :: Expr f 'Regex -> Expr f 'String -> Expr f 'Bool
test re s = Std (Bin StdTest re s)
