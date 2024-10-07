{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module S1 where

import Data.IFunctor (At (..))
import Data.Kind
import Data.Proxy
import Data.Type.Ord
import Foreign
import Foreign.CStorable (CStorable (..))
import GHC.Generics (Generic)
import GHC.TypeNats

type family Alignment (t :: Type) :: Nat
type family Size (t :: Type) :: Nat

-- storInfo :: forall a -> (Storable a) => (Int, Int)
-- storInfo a = (sizeOf @a undefined, alignment @a undefined)

-- foo :: IO ()
-- foo =
--   print
--     [ ("Char", storInfo Char)
--     , ("Bool", storInfo Bool)
--     , ("Int", storInfo Int)
--     , ("Double", storInfo Double)
--     , ("Word8", storInfo Word8)
--     , ("Float", storInfo Float)
--     , ("Word", storInfo Word)
--     , ("Word8", storInfo Word8)
--     , ("Word16", storInfo Word16)
--     , ("Word32", storInfo Word32)
--     , ("Word64", storInfo Word64)
--     ]

type instance Alignment Char = 4
type instance Alignment Bool = 4
type instance Alignment Int = 8
type instance Alignment Double = 8
type instance Alignment Float = 4
type instance Alignment Word = 8
type instance Alignment Word8 = 1
type instance Alignment Word16 = 2
type instance Alignment Word32 = 4
type instance Alignment Word64 = 8

type instance Size Char = 4
type instance Size Bool = 4
type instance Size Int = 8
type instance Size Double = 8
type instance Size Float = 4
type instance Size Word = 8
type instance Size Word8 = 1
type instance Size Word16 = 2
type instance Size Word32 = 4
type instance Size Word64 = 8

{-
padding = (align - (offset mod align)) mod align
aligned = offset + padding
        = offset + ((align - (offset mod align)) mod align)
-}
type family Padding (offset :: Nat) (align :: Nat) :: Nat where
  Padding offset align = (align - (offset `Mod` align)) `Mod` align

type family ListMaxAlignment (s :: Nat) (xs :: [Type]) :: Nat where
  ListMaxAlignment s '[] = s
  ListMaxAlignment s (x ': xs) = ListMaxAlignment (Max s (Alignment x)) xs

type family Acc0 (offset :: Nat) (xs :: [Type]) (bxs :: [Type]) :: [Nat] where
  Acc0 offset '[] bxs = '[offset + Padding offset (ListMaxAlignment 0 bxs)]
  Acc0 offset (x ': xs) bxs =
    (offset + Padding offset (Alignment x))
      ': Acc0 (offset + Padding offset (Alignment x) + Size x) xs bxs

type family Last (xs :: [Nat]) :: Nat where
  Last (x ': '[]) = x
  Last (x ': xs) = Last xs

type family Init (xs :: [Nat]) :: [Nat] where
  Init (x ': '[]) = '[]
  Init (x ': xs) = x ': Init xs

-- | Struct size and field offsets
type family Acc (xs :: [Type]) :: (Nat, [Nat]) where
  Acc xs = '(Last (Acc0 0 xs xs), Init (Acc0 0 xs xs))

class T2L (s :: [Nat]) where
  t2l :: Proxy s -> [Int]

instance T2L '[] where
  t2l _ = []

instance (T2L xs, KnownNat x) => T2L (x ': xs) where
  t2l _ = fromIntegral (natVal (Proxy @x)) : t2l (Proxy @xs)
