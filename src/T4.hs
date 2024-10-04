{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module T4 where

import Data.IFunctor (At (..), IFunctor (..), IMonad (..), returnAt, type (~>))
import qualified Data.IFunctor as I
import Data.Kind
import Data.Proxy
import Data.Type.Map (Insert, InsertOverwriting, Lookup, Map)
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

infixr 4 :&

data Struct :: [Type] -> Type where
  End :: Struct '[]
  (:&) :: a -> Struct as -> Struct (a ': as)

type family
  RMaybe
    (s :: Symbol)
    (vs :: Maybe [Type])
    :: [Type]
  where
  RMaybe s (Just ls) = ls
  RMaybe s Nothing = TypeError (Text "Can't find symbol: " :<>: ShowType s)

type family
  Index
    (n :: Nat)
    (ts :: [Type])
    :: Type
  where
  Index n '[] = TypeError (Text "Too big index")
  Index 0 (x ': _) = x
  Index n (x ': xs) = Index (n - 1) xs

type family UpdateIndex (n :: Nat) (v :: Type) (ts :: [Type]) :: [Type] where
  UpdateIndex n _ '[] = TypeError (Text "Too big index")
  UpdateIndex 0 a (x ': xs) = a ': xs
  UpdateIndex n a (x ': xs) = x ': UpdateIndex (n - 1) a xs

type family PokePF (n :: Nat) (val' :: Type) (val :: Type) (ts :: [Type]) :: [Type] where
  -- PokePF _ NullPtr NullPtr dm = dm
  -- PokePF n NullPtr (ValPtr s) dm = UpdateIndex n (ValPtr s) dm
  -- PokePF n (ValPtr _) NullPtr dm = UpdateIndex n NullPtr dm
  -- PokePF n (ValPtr _) (ValPtr s1) dm = UpdateIndex n (ValPtr s1) dm
  -- PokePF n a a dm = dm
  PokePF _ a b _ =
    TypeError
      ( Text "Poke type error: "
          :<>: Text "expect: "
          :<>: ShowType a
          :<>: Text " actuale: "
          :<>: ShowType b
      )

data MPtr (ia :: DM -> Type) (b :: DM) where
  MReturn :: ia c -> MPtr ia c
  NewPtr
    :: Proxy (s :: Symbol)
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
  PokePtrField
    :: ValPtr s
    -> Proxy (n :: Nat)
    -> val
    -> MPtr
        ia
        ( InsertOverwriting
            s
            ( PokePF
                n
                (Index n (RMaybe s (Lookup s dm)))
                val
                (RMaybe s (Lookup s dm))
            )
            dm
        )
    -> MPtr ia dm
  LiftM :: IO (MPtr ia dm) -> MPtr ia dm

instance IFunctor MPtr where
  imap f = \case
    MReturn a -> MReturn (f a)
    NewPtr s st contF -> NewPtr s st (imap f . contF)
    PeekPtr vs contF -> PeekPtr vs (imap f . contF)
    PeekPtrField vs n contF -> PeekPtrField vs n (imap f . contF)
    PokePtrField vs n val cont -> PokePtrField vs n val (imap f cont)
    LiftM ma -> LiftM (fmap (imap f) ma)

instance IMonad MPtr where
  ireturn = MReturn
  ibind f = \case
    MReturn a -> f a
    NewPtr s st contF -> NewPtr s st (ibind f . contF)
    PeekPtr vs contF -> PeekPtr vs (ibind f . contF)
    PeekPtrField vs n contF -> PeekPtrField vs n (ibind f . contF)
    PokePtrField vs n val cont -> PokePtrField vs n val (ibind f cont)
    LiftM ma -> LiftM (fmap (ibind f) ma)

newptr
  :: forall (s :: Symbol)
    ->(Nothing ~ Lookup s dm)
  => Struct ts -> MPtr (At (ValPtr s) (Insert s ts dm)) dm
newptr s st = NewPtr (Proxy @s) st ireturn

peekptr :: ValPtr s -> MPtr (At (Struct (RMaybe s (Lookup s dm))) dm) dm
peekptr vs = PeekPtr vs ireturn

peekptrf
  :: ValPtr s -> forall (n :: Nat) -> MPtr (At (Index n (RMaybe s (Lookup s dm))) dm) dm
peekptrf vps n = PeekPtrField vps (Proxy @n) ireturn

pokeptrf
  :: forall (n :: Nat) s val dm
   . ValPtr s
  -> Proxy (n)
  -> val
  -> MPtr
      ( At
          ()
          ( InsertOverwriting
              s
              ( PokePF
                  n
                  (Index n (RMaybe s (Lookup s dm)))
                  val
                  (RMaybe s (Lookup s dm))
              )
              dm
          )
      )
      dm
pokeptrf vps n val = PokePtrField vps (n) val (returnAt ())

foo :: MPtr (At () '[]) '[]
foo = I.do
  At k1 <- newptr "k1" (True :& "st" :& NullPtrC :& End)
  At k2 <- newptr "k2" (True :& (1 :: Double) :& End)
  At v <- peekptr k1
  At v1 <- peekptrf k1 1
  LiftM $ print v1 >> pure (returnAt ())
  pokeptrf k1 (Proxy @1) k2
  pokeptrf k1 (Proxy @2) k2
  pokeptrf k1 (Proxy @2) k2
  pokeptrf k1 (Proxy @1) k2
  -- pokeptrf k1 2 "st"
  -- pokeptrf k1 2 k2
  -- pokeptrf k1 2 k2
  -- pokeptrf k1 2 k2
  -- pokeptrf k1 2 k2
  undefined
