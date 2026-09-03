{-# LANGUAGE OverloadedStrings #-}

-- | JSDoc + @export function@ / @function@ extraction for plain JS.
module JShark.Bindgen.ParseJs
  ( parseJs
  )
where

import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import JShark.Bindgen.Ir

parseJs :: Text -> Text -> Text -> Either String ModuleIr
parseJs moduleName sourceName src =
  Right (parseJsdocFuns moduleName sourceName src)

parseJsdocFuns :: Text -> Text -> Text -> ModuleIr
parseJsdocFuns moduleName sourceName src =
  (emptyModule moduleName sourceName)
    { irFuns = go (T.lines src) Nothing
    }
 where
  go [] _ = []
  go (ln : rest) doc
    | Just d <- T.stripPrefix "/**" (T.stripStart ln) =
        let
          (block, rest') = takeDoc d rest
         in
          go rest' (Just (parseDoc block))
    | Just f <- parseFunLine ln =
        let
          (params, ret) = maybe (guessParams f, TyUnknown "js") id doc
          named =
            zipWith
              (\p ty -> p {pTy = ty})
              (fnParams f)
              (params <> repeat (TyUnknown "js"))
         in
          f {fnParams = named, fnRet = ret} : go rest Nothing
    | otherwise = go rest doc

takeDoc :: Text -> [Text] -> (Text, [Text])
takeDoc start rest =
  case T.breakOn "*/" start of
    (a, b)
      | "*/" `T.isInfixOf` start ->
          (T.strip (T.replace "*/" "" (a <> T.take 2 b)), rest)
    _ ->
      let
        (more, rest') = spanLines rest
       in
        (T.unlines (start : more), rest')
 where
  spanLines [] = ([], [])
  spanLines (ln : xs)
    | "*/" `T.isInfixOf` ln =
        ([T.strip (fst (T.breakOn "*/" ln))], xs)
    | otherwise =
        let
          (ms, r) = spanLines xs
         in
          (T.strip ln : ms, r)

parseDoc :: Text -> ([Ty], Ty)
parseDoc t =
  let
    ls = fmap T.strip (T.lines t)
    params =
      [ parseTyTok ty
      | ln <- ls
      , Just rest <- [T.stripPrefix "@param" (stripStar ln)]
      , let
          ty = jsdocType rest
      ]
    ret =
      case [parseTyTok ty | ln <- ls, Just ty <- [returnTy ln]] of
        r : _ -> r
        [] -> TyUnknown "js"
   in
    (params, ret)

returnTy :: Text -> Maybe Text
returnTy ln =
  let
    s = stripStar ln
   in
    if "@returns" `T.isPrefixOf` s
      then Just (jsdocType (T.dropWhile isSpace (T.drop 8 s)))
      else
        if "@return" `T.isPrefixOf` s
          then case T.uncons (T.drop 7 s) of
            Just ('s', _) -> Nothing
            _ -> Just (jsdocType (T.dropWhile isSpace (T.drop 7 s)))
          else Nothing

stripStar :: Text -> Text
stripStar = T.dropWhile (\c -> isSpace c || c == '*')

jsdocType :: Text -> Text
jsdocType t =
  let
    t1 = T.strip t
   in
    case T.uncons t1 of
      Just ('{', r) -> T.takeWhile (/= '}') r
      _ -> T.empty

parseTyTok :: Text -> Ty
parseTyTok raw =
  let
    t = T.toLower (T.strip raw)
   in
    case t of
      "number" -> TyNumber
      "string" -> TyString
      "boolean" -> TyBool
      "bool" -> TyBool
      "void" -> TyUnit
      "undefined" -> TyUnit
      "bigint" -> TyBigInt
      "uint8array" -> TyUint8Array
      _
        | "[" `T.isSuffixOf` t ->
            TyArray (parseTyTok (T.dropEnd 2 t))
        | T.isPrefixOf "array<" t ->
            TyArray (parseTyTok (T.dropEnd 1 (T.drop 6 t)))
        | T.isPrefixOf "promise<" t ->
            TyPromise (parseTyTok (T.dropEnd 1 (T.drop 8 t)))
        | T.null t -> TyUnknown "js"
        | otherwise -> TyNamed (T.strip raw)

parseFunLine :: Text -> Maybe Fun
parseFunLine ln =
  let
    t = T.strip ln
    t1 = fromMaybe t (T.stripPrefix "export" t >>= stripSpace)
    t2 = fromMaybe t1 (T.stripPrefix "async" t1 >>= stripSpace)
    t3 = fromMaybe t2 (T.stripPrefix "function" t2 >>= stripSpace)
   in
    case () of
      _
        | T.isPrefixOf "function" (T.strip t1)
        , Just f <- funAfter (T.strip t3) ->
            Just f
        | T.isPrefixOf "function" (T.strip ln)
        , Just f <- funAfter t3 ->
            Just f
        | otherwise -> constFun t
 where
  stripSpace x =
    let
      y = T.dropWhile isSpace x
     in
      if T.null y then Nothing else Just y
  fromMaybe a m = maybe a id m

funAfter :: Text -> Maybe Fun
funAfter t =
  let
    name = T.takeWhile isIdent t
    rest = T.dropWhile isIdent t
   in
    case T.uncons (T.dropWhile isSpace rest) of
      Just ('(', r) ->
        let
          args = T.takeWhile (/= ')') r
          names =
            [ T.takeWhile isIdent (T.strip a)
            | a <- T.splitOn "," args
            , not (T.null (T.strip a))
            ]
         in
          if T.null name
            then Nothing
            else
              Just
                Fun
                  { fnName = name
                  , fnFfi = name
                  , fnParams =
                      [ Param n (TyUnknown "js") False
                      | n <- names
                      , not (T.null n)
                      ]
                  , fnRet = TyUnknown "js"
                  , fnIsCtor = False
                  , fnStatic = False
                  }
      _ -> Nothing

constFun :: Text -> Maybe Fun
constFun t =
  let
    t1 = fromMaybe t (T.stripPrefix "export" (T.strip t) >>= justStrip)
   in
    case T.stripPrefix "const" t1 of
      Just rest ->
        let
          rest1 = T.dropWhile isSpace rest
          name = T.takeWhile isIdent rest1
          after = T.dropWhile isSpace (T.dropWhile isIdent rest1)
         in
          case T.uncons after of
            Just ('=', r)
              | "(" `T.isInfixOf` r && "=>" `T.isInfixOf` r ->
                  funAfter (name <> T.dropWhile (/= '(') r)
            _ -> Nothing
      Nothing -> Nothing
 where
  fromMaybe a m = maybe a id m
  justStrip x =
    let
      y = T.dropWhile isSpace x
     in
      if T.null y then Nothing else Just y

guessParams :: Fun -> [Ty]
guessParams f = fmap (const (TyUnknown "js")) (fnParams f)

isIdent :: Char -> Bool
isIdent c = isAlphaNum c || c == '_' || c == '$' || isAlpha c
