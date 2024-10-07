{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TypedPtr.Core where

import Data.IFunctor (At (..), IFunctor (..), IMonad (..), returnAt, type (~>))
import Data.Kind
import Data.Proxy
import Data.Type.Map (Delete, Insert, InsertOverwriting, Lookup)
import Foreign
import GHC.TypeLits
import TypedPtr.Storable
import TypedPtr.Type

data MPtr (ia :: DM -> Type) (b :: DM) where
  MReturn :: ia c -> MPtr ia c
  NewPtr
    :: ( CheckNothing (Lookup s dm) s "Already existing ptr: "
       , KnownNat (ListMaxAlignment 0 ts)
       , KnownNat (Last (Acc0 0 ts ts))
       , T2L (Init (Acc0 0 ts ts))
       , PeekStruct ts
       )
    => Proxy (s :: Symbol)
    -> Struct ts
    -> (At (StructPtr s) (Insert s ts dm) ~> MPtr ia)
    -> MPtr ia dm
  PeekPtr
    :: ( CheckJust (Lookup s dm) s "Peek freed ptr: "
       , ts ~ (FromJust (Lookup s dm))
       , KnownNat (ListMaxAlignment 0 ts)
       , KnownNat (Last (Acc0 0 ts ts))
       , T2L (Init (Acc0 0 ts ts))
       , PeekStruct ts
       )
    => StructPtr s
    -> (At (Struct ts) dm ~> MPtr ia)
    -> MPtr ia dm
  PeekPtrField
    :: ( CheckJust (Lookup s dm) s "Peek freed ptr: "
       , ts ~ FromJust (Lookup s dm)
       , val ~ Index n ts
       , offset ~ Index n (Init (Acc0 0 ts ts))
       , Storable val
       , KnownNat offset
       )
    => StructPtr s
    -> Proxy (n :: Nat)
    -> Proxy (offset :: Nat)
    -> (At val dm ~> MPtr ia)
    -> MPtr ia dm
  PokePtrField
    :: ( CheckJust (Lookup s dm) s "Poke freed ptr: "
       , ts ~ FromJust (Lookup s dm)
       , val' ~ Index n ts
       , Check val' val
       , newts ~ UpdateIndex n val ts
       , newdm ~ InsertOverwriting s newts dm
       , offset ~ Index n (Init (Acc0 0 ts ts))
       , Storable val
       , KnownNat offset
       )
    => StructPtr s
    -> Proxy (n :: Nat)
    -> Proxy (offset :: Nat)
    -> val
    -> MPtr ia newdm
    -> MPtr ia dm
  FreePtr
    :: ( CheckJust (Lookup s dm) s ("Double free ptr: ")
       , newdm ~ DeleteVal (StructPtr s) (Delete s dm)
       )
    => StructPtr s
    -> MPtr ia newdm
    -> MPtr ia dm
  LiftM :: IO (MPtr ia dm) -> MPtr ia dm

instance IFunctor MPtr where
  imap f = \case
    MReturn a -> MReturn (f a)
    NewPtr s st contF -> NewPtr s st (imap f . contF)
    PeekPtr vs contF -> PeekPtr vs (imap f . contF)
    PeekPtrField vs n offset contF -> PeekPtrField vs n offset (imap f . contF)
    PokePtrField vs n offset val cont -> PokePtrField vs n offset val (imap f cont)
    FreePtr vs cont -> FreePtr vs (imap f cont)
    LiftM ma -> LiftM (fmap (imap f) ma)

instance IMonad MPtr where
  ireturn = MReturn
  ibind f = \case
    MReturn a -> f a
    NewPtr s st contF -> NewPtr s st (ibind f . contF)
    PeekPtr vs contF -> PeekPtr vs (ibind f . contF)
    PeekPtrField vs n offset contF -> PeekPtrField vs n offset (ibind f . contF)
    PokePtrField vs n offset val cont -> PokePtrField vs n offset val (ibind f cont)
    FreePtr vs cont -> FreePtr vs (ibind f cont)
    LiftM ma -> LiftM (fmap (ibind f) ma)

newptr
  :: forall (s :: Symbol)
    ->( CheckNothing (Lookup s dm) s "Already existing ptr: "
      , KnownNat (ListMaxAlignment 0 ts)
      , KnownNat (Last (Acc0 0 ts ts))
      , T2L (Init (Acc0 0 ts ts))
      , PeekStruct ts
      )
  => Struct ts -> MPtr (At (StructPtr s) (Insert s ts dm)) dm
newptr s st = NewPtr (Proxy @s) st ireturn

peekptr
  :: ( CheckJust (Lookup s dm) s "Peek freed ptr: "
     , ts ~ (FromJust (Lookup s dm))
     , KnownNat (ListMaxAlignment 0 ts)
     , KnownNat (Last (Acc0 0 ts ts))
     , T2L (Init (Acc0 0 ts ts))
     , PeekStruct ts
     )
  => StructPtr s -> MPtr (At (Struct (FromJust (Lookup s dm))) dm) dm
peekptr vs = PeekPtr vs ireturn

peekptrf
  :: forall s dm ts val offset
   . StructPtr s
  -> forall (n :: Nat)
    ->( CheckJust (Lookup s dm) s "Peek freed ptr: "
      , Just ts ~ Lookup s dm
      , val ~ Index n ts
      , Storable val
      , offset ~ Index n (Init (Acc0 0 ts ts))
      , KnownNat offset
      )
  => MPtr (At (Index n ts) dm) dm
peekptrf vps n =
  PeekPtrField
    vps
    (Proxy @n)
    (Proxy @offset)
    ireturn

pokeptrf
  :: forall s val dm ts val' newts newdm offset
   . StructPtr s
  -> forall (n :: Nat)
    ->val
  -> ( CheckJust (Lookup s dm) s "Poke freed ptr: "
     , ts ~ FromJust (Lookup s dm)
     , val' ~ Index n ts
     , Check val' val
     , newts ~ UpdateIndex n val ts
     , newdm ~ InsertOverwriting s newts dm
     , offset ~ Index n (Init (Acc0 0 ts ts))
     , Storable val
     , KnownNat offset
     )
  => MPtr (At () newdm) dm
pokeptrf vps n val = PokePtrField vps (Proxy @n) (Proxy @offset) val (returnAt ())

freeptr
  :: ( CheckJust (Lookup s dm) s ("Double free ptr: ")
     , newdm ~ DeleteVal (StructPtr s) (Delete s dm)
     )
  => StructPtr s
  -> MPtr (At () (DeleteVal (StructPtr s) (Delete s dm))) dm
freeptr vs = FreePtr vs (returnAt ())

liftm :: IO a -> MPtr (At a dm) dm
liftm ma = LiftM (ma >>= pure . returnAt)

runMPtr :: MPtr (At a dm') dm -> IO a
runMPtr = \case
  MReturn (At a) -> pure a
  NewPtr _ (st :: Struct ts) cont -> do
    ptr <- malloc @(Struct ts)
    poke ptr st
    runMPtr (cont (At (StructPtrC ptr)))
  PeekPtr (StructPtrC ptr) (cont :: At (Struct ts) dma ~> MPtr (At a dmb)) -> do
    val <- peek @(Struct ts) (castPtr ptr)
    runMPtr (cont (At val))
  PeekPtrField
    (StructPtrC ptr)
    (Proxy :: Proxy n)
    (Proxy :: Proxy offset)
    (cont :: At val dma ~> MPtr ia) -> do
      let offset = fromIntegral $ natVal (Proxy @offset)
      val <- peek @val (castPtr (ptr `plusPtr` offset))
      runMPtr (cont (At val))
  PokePtrField
    (StructPtrC ptr)
    (Proxy :: Proxy n)
    (Proxy :: Proxy offset)
    (val :: val)
    cont -> do
      let offset = fromIntegral $ natVal (Proxy @offset)
      poke @val (castPtr (ptr `plusPtr` offset)) val
      runMPtr cont
  FreePtr (StructPtrC ptr) cont -> do
    free ptr
    runMPtr cont
  LiftM mcont -> do
    m <- mcont
    runMPtr m
