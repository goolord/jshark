{-# LANGUAGE LambdaCase #-}

-- | Flat opcode column encoding helpers (no dependency on 'JShark.Flat').
module JShark.FlatEnc
  ( module JShark.FlatEnc
  )
where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Int (Int32)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import Data.Word (Word16)

type Op = Word16

data Enc = Enc !Op !Int32 !Int32 !Int32 !Int32 !Int32

oFE_LITERAL :: Op
oFE_LITERAL = 1

oFE_VAR :: Op
oFE_VAR = 2

oFE_LET :: Op
oFE_LET = 3

oFE_LETREC :: Op
oFE_LETREC = 4

oFE_LAMBDA :: Op
oFE_LAMBDA = 5

oFE_APPLY :: Op
oFE_APPLY = 6

oFE_EMBEDEFF :: Op
oFE_EMBEDEFF = 7

oFE_IF :: Op
oFE_IF = 8

oFE_OPTIONCASE :: Op
oFE_OPTIONCASE = 9

oFE_RESOK :: Op
oFE_RESOK = 10

oFE_RESERR :: Op
oFE_RESERR = 11

oFE_RESCASE :: Op
oFE_RESCASE = 12

oFE_INDEX :: Op
oFE_INDEX = 13

oFE_U8INDEX :: Op
oFE_U8INDEX = 14

oFE_ERROR :: Op
oFE_ERROR = 15

oFE_FIXED :: Op
oFE_FIXED = 16

oFE_FNLIT :: Op
oFE_FNLIT = 17

oFE_FROZEN :: Op
oFE_FROZEN = 18

oFE_GETFIELD :: Op
oFE_GETFIELD = 19

oFE_UNSAFENULL :: Op
oFE_UNSAFENULL = 20

oFE_KCONCAT :: Op
oFE_KCONCAT = 21

oFE_KPLUS :: Op
oFE_KPLUS = 22

oFE_KTIMES :: Op
oFE_KTIMES = 23

oFE_KMINUS :: Op
oFE_KMINUS = 24

oFE_KNEG :: Op
oFE_KNEG = 25

oFE_KDIV :: Op
oFE_KDIV = 26

oFE_KREM :: Op
oFE_KREM = 27

oFE_KBITAND :: Op
oFE_KBITAND = 28

oFE_KBITOR :: Op
oFE_KBITOR = 29

oFE_KBITXOR :: Op
oFE_KBITXOR = 30

oFE_KSHL :: Op
oFE_KSHL = 31

oFE_KSHR :: Op
oFE_KSHR = 32

oFE_KUSHR :: Op
oFE_KUSHR = 33

oFE_KBIG :: Op
oFE_KBIG = 34

oFE_KBIGNEG :: Op
oFE_KBIGNEG = 35

oFE_KAND :: Op
oFE_KAND = 36

oFE_KOR :: Op
oFE_KOR = 37

oFE_KEQ :: Op
oFE_KEQ = 38

oFE_KNEQ :: Op
oFE_KNEQ = 39

oFE_KGTH :: Op
oFE_KGTH = 40

oFE_KLTH :: Op
oFE_KLTH = 41

oFE_KGTEQ :: Op
oFE_KGTEQ = 42

oFE_KLTEQ :: Op
oFE_KLTEQ = 43

oFE_KSHOW :: Op
oFE_KSHOW = 44

oFE_KTYPEOF :: Op
oFE_KTYPEOF = 45

oFE_MMAP :: Op
oFE_MMAP = 46

oFE_MFILTER :: Op
oFE_MFILTER = 47

oFE_MREDUCE :: Op
oFE_MREDUCE = 48

oFE_MREDUCER :: Op
oFE_MREDUCER = 49

oFE_MTOSORTED :: Op
oFE_MTOSORTED = 50

oFE_MFROM :: Op
oFE_MFROM = 51

oFE_HVM2REF :: Op
oFE_HVM2REF = 52

oFX_LIFT :: Op
oFX_LIFT = 100

oFX_FFI :: Op
oFX_FFI = 101

oFX_UNSAFEOBJ :: Op
oFX_UNSAFEOBJ = 102

oFX_UNSAFEOBJGET :: Op
oFX_UNSAFEOBJGET = 103

oFX_UNSAFEOBJSET :: Op
oFX_UNSAFEOBJSET = 104

oFX_CALLMETHOD :: Op
oFX_CALLMETHOD = 105

oFX_BIND :: Op
oFX_BIND = 106

oFX_THENE :: Op
oFX_THENE = 107

oFX_BINDREC :: Op
oFX_BINDREC = 108

oFX_LAMBDAE :: Op
oFX_LAMBDAE = 109

oFX_APPLYE :: Op
oFX_APPLYE = 110

oFX_IFE :: Op
oFX_IFE = 111

oFX_WHILE :: Op
oFX_WHILE = 112

oFX_FORRANGE :: Op
oFX_FORRANGE = 113

oFX_U8SET :: Op
oFX_U8SET = 114

oFX_U8FILL :: Op
oFX_U8FILL = 115

oFX_OPTCASEE :: Op
oFX_OPTCASEE = 116

oFX_RESCASEE :: Op
oFX_RESCASEE = 117

oFX_STRCASEE :: Op
oFX_STRCASEE = 118

oFX_THROW :: Op
oFX_THROW = 119

oFX_TRY :: Op
oFX_TRY = 120

oFX_OBJLIT :: Op
oFX_OBJLIT = 121

oFX_DELETEPROP :: Op
oFX_DELETEPROP = 122

oFX_ARRAYLIT :: Op
oFX_ARRAYLIT = 123

-- | Freeze enc rows in pack order (row @i@ is the @i@th append).
freezeEncSeq ::
  Seq Enc
  -> ( VU.Vector Op
     , VU.Vector Int32
     , VU.Vector Int32
     , VU.Vector Int32
     , VU.Vector Int32
     , VU.Vector Int32
     )
freezeEncSeq encs =
  let
    n = Seq.length encs
   in
    runST $ do
      opM <- MVU.new n
      aM <- MVU.new n
      bM <- MVU.new n
      cM <- MVU.new n
      dM <- MVU.new n
      eM <- MVU.new n
      forM_ [0 .. n - 1] $ \i ->
        case Seq.index encs i of
          Enc o a b c d e -> do
            MVU.write opM i o
            MVU.write aM i a
            MVU.write bM i b
            MVU.write cM i c
            MVU.write dM i d
            MVU.write eM i e
      opF <- VU.unsafeFreeze opM
      aF <- VU.unsafeFreeze aM
      bF <- VU.unsafeFreeze bM
      cF <- VU.unsafeFreeze cM
      dF <- VU.unsafeFreeze dM
      eF <- VU.unsafeFreeze eM
      pure (opF, aF, bF, cF, dF, eF)

-- | Freeze @[(Enc)]@ to column vectors in list order: row @i@ is @encs !! i@.
freezeEncColumns ::
  [Enc]
  -> ( VU.Vector Op
     , VU.Vector Int32
     , VU.Vector Int32
     , VU.Vector Int32
     , VU.Vector Int32
     , VU.Vector Int32
     )
freezeEncColumns encs =
  ( column op
  , column a
  , column b
  , column c
  , column d
  , column e
  )
 where
  column f = VU.fromList [f row | row <- encs]
  op (Enc o _ _ _ _ _) = o
  a (Enc _ x _ _ _ _) = x
  b (Enc _ _ x _ _ _) = x
  c (Enc _ _ _ x _ _) = x
  d (Enc _ _ _ _ x _) = x
  e (Enc _ _ _ _ _ x) = x

-- | Regression guard: 'freezeEncColumns' must not permute rows.
freezeEncColumnsOrderOk :: Bool
freezeEncColumnsOrderOk =
  let
    encs =
      [ Enc 22 1 2 3 4 5
      , Enc 1 99 0 0 0 0
      , Enc 2 7 0 0 0 0
      ]
    (ops, as, bs, cs, ds, es) = freezeEncColumns encs
   in
    VU.toList ops == [22, 1, 2]
      && VU.toList as == [1, 99, 7]
      && VU.toList bs == [2, 0, 0]
      && VU.toList cs == [3, 0, 0]
      && VU.toList ds == [4, 0, 0]
      && VU.toList es == [5, 0, 0]
