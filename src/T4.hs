{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module T4 where

import Data.IFunctor (At (..), type (~>))
import Data.Kind
import Data.Proxy
import Data.Type.Map (Insert, Lookup, Map)
import Data.Void
import Foreign
import GHC.TypeLits

type DM = Map Symbol [Type]

data NullPtr = NullPtrC
instance Show NullPtr where
  show _ = "NullPtr"

data ValPtr (s :: Symbol) = forall a. ValPtrC (Ptr a)

instance Show (ValPtr s) where
  show (ValPtrC ptr) = show ptr

instance Storable NullPtr where
  sizeOf _ = 8
  alignment _ = 8
  peek _ = pure NullPtrC
  poke _ _ = pure ()

instance Storable (ValPtr a) where
  sizeOf _ = 8
  alignment _ = 8
  peek ptr = do
    val <- peek (castPtr ptr)
    pure (ValPtrC val)
  poke ptr (ValPtrC nptr) = poke (castPtr ptr) nptr

data Struct :: [Type] -> Type where
  End :: Struct '[]
  Cons :: a -> Struct as -> Struct (a ': as)

type family
  RMaybe
    (s :: Symbol)
    (vs :: Maybe [Type])
    :: [Type]
  where
  RMaybe s Nothing =
    TypeError (Text "Can't find symbol: " :<>: ShowType s)
  RMaybe _ (Just ls) = ls

type family
  Index
    (n :: Nat)
    (ts :: [Type])
    :: Type
  where
  Index 0 '[] = TypeError (Text "Too big index: " :<>: ShowType 0)
  Index n (x ': xs) = Index (n - 1) xs

data MPtr (ia :: DM -> Type) (b :: DM) where
  MReturn :: ia c -> MPtr ia c
  NewPtr
    :: (Nothing ~ Lookup s dm)
    => Proxy (s :: Symbol)
    -> Struct ts
    -> (At (ValPtr s) (Insert s ts dm) ~> MPtr ia)
    -> MPtr ia dm
  PeekPtr
    :: ValPtr s
    -> (At (Struct (RMaybe s (Lookup s dm))) dm ~> MPtr ia)
    -> MPtr ia dm
  PeekPtrField
    :: ValPtr s
    -> Proxy (n :: Nat)
    -> (At (Index n (RMaybe s (Lookup s dm))) dm ~> MPtr ia)
    -> MPtr ia dm
  PokePtr
    :: ValPtr s
    -> Struct ts
    -> MPtr ia dm
    -> MPtr ia dm
  LiftM :: IO (MPtr ia dm) -> MPtr ia dm
