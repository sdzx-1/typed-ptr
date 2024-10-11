{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.TypedPtr.Storable where

import Data.Kind
import Data.Proxy
import Data.Type.Map (type (:->) (..))
import Data.Word
import Foreign
import GHC.TypeLits (KnownNat, KnownSymbol)
import GHC.TypeNats (natVal)
import Test.QuickCheck
import TypedPtr

instance Arbitrary (Struct ('[])) where
  arbitrary = pure End

instance
  ( Arbitrary x
  , Storable x
  , Arbitrary (Struct xs)
  , KnownSymbol anySym
  )
  => Arbitrary (Struct ((anySym ':-> x) ': xs))
  where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    pure ((Proxy, x) :& xs)

instance (KnownNat size, Arbitrary a) => Arbitrary (Array size a) where
  arbitrary = do
    ls <- sequence [arbitrary @a | _ <- [1 .. (natVal (Proxy @size))]]
    pure (ArrayC ls)
