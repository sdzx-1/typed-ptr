{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Data.IFunctor (At (..), returnAt)
import qualified Data.IFunctor as I
import Data.Kind
import Data.Proxy
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

    it "TestNestStruct1" $
      property (prop_storableStruct TestNestStruct1)

    it "PeekPtr" $
      property prop_PeekPtr

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
   ]

type TestMaybeStruct =
  '[ "field1" ':-> Maybe Char
   , "field2" ':-> Maybe Bool
   , "field3" ':-> Maybe Int
   , "field4" ':-> Maybe Double
   , "field5" ':-> Maybe Float
   ]

type TestArrayStruct =
  '[ "field1" ':-> Array 1 Char
   , "field2" ':-> Array 2 Bool
   , "field3" ':-> Array 3 Int
   , "field4" ':-> Array 4 Double
   , "field5" ':-> Array 5 Float
   ]

type TestMixStruct =
  '[ "field1" ':-> Char
   , "field2" ':-> Array 2 Bool
   , "field3" ':-> Maybe Int
   , "field4" ':-> Array 4 Double
   , "field5" ':-> Float
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
   ]

type TestNestStruct1 =
  '[ "a"
      ':-> Struct
            '[ "b"
                ':-> Struct '["c" ':-> Bool]
             , "b1" ':-> Word8
             , "b2" ':-> Struct '["c" ':-> Int]
             , "b3" ':-> Array 3 Word8
             ]
   , "b" ':-> Int
   ]

prop_PeekPtr :: Property
prop_PeekPtr = monadicIO $ do
  struct <- pick (arbitrary @(Struct TestNestStruct1))
  struct' <- run $ runMPtr @(Struct TestNestStruct1) @'[] @'[] $ I.do
    At testPtr <- newStructPtr "test" TestNestStruct1 struct
    At val0 <- peekPtr testPtr.a.b.c
    At val1 <- peekPtr testPtr.a.b1
    At val2 <- peekPtr testPtr.a.b2.c
    At val3 <- peekPtr testPtr.a.b3
    At val4 <- peekPtr testPtr.b
    freeptr testPtr
    returnAt
      ( ( Proxy
        , ( (Proxy, (Proxy, val0) :& End)
              :& (Proxy, val1)
              :& (Proxy, (Proxy, val2) :& End)
              :& (Proxy, val3)
              :& End
          )
        )
          :& (Proxy, val4)
          :& End
      )
  assert $ struct == struct'
