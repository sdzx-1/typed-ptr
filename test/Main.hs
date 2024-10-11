{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Data.Kind
import Data.Type.Map ((:->) (..))
import Foreign
import GHC.TypeLits
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.TypedPtr.Storable
import TypedPtr
import TypedPtr.Type

main :: IO ()
main = hspec $ do
  describe "Storable Struct poke and peek" $ do
    it "TestValStruct" $
      property (prop_storableStruct TestValStruct)

    it "TestMaybeStruct" $
      property (prop_storableStruct TestMaybeStruct)

    it "TestArrayStruct" $
      property (prop_storableStruct TestArrayStruct)

    it "TestMixStruct" $
      property (prop_storableStruct TestMixStruct)

    it "TestNestStruct" $
      property (prop_storableStruct TestNestStruct)

prop_storableStruct
  :: forall (ts :: [Symbol :-> Type])
    ->( Arbitrary (Struct ts)
      , All Show ts
      , All Eq ts
      , (KnownNat (ListMaxAlignment 0 ts))
      , KnownNat (Last (Acc0 0 ts ts))
      , ReifyOffsets (Init (Acc0 0 ts ts))
      , PeekStruct ts
      )
  => Property
prop_storableStruct ts = monadicIO $ do
  str <- pick (arbitrary @(Struct ts))
  ptr <- run $ malloc @(Struct ts)
  run $ poke ptr str
  str1 <- run $ peek ptr
  run $ free ptr
  assert $ str == str1

type TestValStruct =
  '[ "field1" ':-> Char
   , "field2" ':-> Bool
   , "field3" ':-> Int
   , "field4" ':-> Double
   , "field5" ':-> Float
   , "field6" ':-> Word8
   , "field7" ':-> Word16
   , "field8" ':-> Word32
   , "field9" ':-> Word64
   ]

type TestMaybeStruct =
  '[ "field1" ':-> Maybe Char
   , "field2" ':-> Maybe Bool
   , "field3" ':-> Maybe Int
   , "field4" ':-> Maybe Double
   , "field5" ':-> Maybe Float
   , "field6" ':-> Maybe Word8
   , "field7" ':-> Maybe Word16
   , "field8" ':-> Maybe Word32
   , "field9" ':-> Maybe Word64
   ]

type TestArrayStruct =
  '[ "field1" ':-> Array 1 Char
   , "field2" ':-> Array 2 Bool
   , "field3" ':-> Array 3 Int
   , "field4" ':-> Array 4 Double
   , "field5" ':-> Array 5 Float
   , "field6" ':-> Array 6 Word8
   , "field7" ':-> Array 7 Word16
   , "field8" ':-> Array 8 Word32
   , "field9" ':-> Array 9 Word64
   ]

type TestMixStruct =
  '[ "field1" ':-> Char
   , "field2" ':-> Array 2 Bool
   , "field3" ':-> Maybe Int
   , "field4" ':-> Array 4 Double
   , "field5" ':-> Float
   , "field6" ':-> Word8
   , "field7" ':-> Array 7 Word16
   , "field8" ':-> Maybe Word32
   , "field9" ':-> Array 9 Word64
   ]
type TestNestStruct =
  '[ "field1" ':-> Char
   , "field2" ':-> Array 2 Bool
   , "field3" ':-> Maybe Int
   , "field4"
      ':-> Struct
            '[ "f1" ':-> Int
             , "f2" ':-> Bool
             ]
   , "field5" ':-> Float
   , "field6" ':-> Word8
   , "field7" ':-> Array 7 Word16
   , "field8" ':-> Maybe Word32
   , "field9" ':-> Array 9 Word64
   ]
