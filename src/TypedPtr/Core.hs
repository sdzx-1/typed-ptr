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
import Data.Type.Map (Delete, Insert, InsertOverwriting, Lookup, (:->) (..))
import Foreign
import GHC.TypeLits
import TypedPtr.Storable
import TypedPtr.Type

data MPtr (ia :: DM -> Type) (b :: DM) where
  MReturn :: ia c -> MPtr ia c
  NewPtr
    :: ( CheckNothing (Lookup s dm) s "Already existing ptr: "
       , ts ~ CollVal sts
       , KnownNat (ListMaxAlignment 0 ts)
       , KnownNat (Last (Acc0 0 ts ts))
       , T2L (Init (Acc0 0 ts ts))
       , PeekStruct ts
       )
    => Proxy (s :: Symbol)
    -> Proxy (sts :: [Symbol :-> Type])
    -> Struct ts
    -> (At (StructPtr s) (Insert s sts dm) ~> MPtr ia)
    -> MPtr ia dm
  PeekPtr
    :: ( CheckJust (Lookup s dm) s "Peek freed ptr: "
       , ts ~ CollVal (FromJust (Lookup s dm))
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
       , CheckField s sym sts
       , sts ~ FromJust (Lookup s dm)
       , ts ~ CollVal sts
       , n ~ LookupField sym 0 sts
       , val ~ Index n ts
       , offset ~ Index n (Init (Acc0 0 ts ts))
       , Storable val
       , KnownNat offset
       )
    => StructPtr s
    -> Proxy (sym :: Symbol)
    -> Proxy (offset :: Nat)
    -> (At val dm ~> MPtr ia)
    -> MPtr ia dm
  PokePtrField
    :: ( CheckJust (Lookup s dm) s "Poke freed ptr: "
       , CheckField s sym0 sts
       , sts ~ (FromJust (Lookup s dm))
       , ts ~ CollVal sts
       , n ~ LookupField sym0 0 sts
       , (sym ':-> val') ~ Index n sts
       , Check val' val
       , newts ~ UpdateIndex n (sym ':-> val) sts
       , newdm ~ InsertOverwriting s newts dm
       , offset ~ Index n (Init (Acc0 0 ts ts))
       , Storable val
       , KnownNat offset
       )
    => StructPtr s
    -> Proxy (sym0 :: Symbol)
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
    NewPtr s sts st contF -> NewPtr s sts st (imap f . contF)
    PeekPtr vs contF -> PeekPtr vs (imap f . contF)
    PeekPtrField vs n offset contF -> PeekPtrField vs n offset (imap f . contF)
    PokePtrField vs n offset val cont -> PokePtrField vs n offset val (imap f cont)
    FreePtr vs cont -> FreePtr vs (imap f cont)
    LiftM ma -> LiftM (fmap (imap f) ma)

instance IMonad MPtr where
  ireturn = MReturn
  ibind f = \case
    MReturn a -> f a
    NewPtr s sts st contF -> NewPtr s sts st (ibind f . contF)
    PeekPtr vs contF -> PeekPtr vs (ibind f . contF)
    PeekPtrField vs n offset contF -> PeekPtrField vs n offset (ibind f . contF)
    PokePtrField vs n offset val cont -> PokePtrField vs n offset val (ibind f cont)
    FreePtr vs cont -> FreePtr vs (ibind f cont)
    LiftM ma -> LiftM (fmap (ibind f) ma)

newptr
  :: forall (s :: Symbol)
    ->forall (sts :: [Symbol :-> Type])
    ->( CheckNothing (Lookup s dm) s "Already existing ptr: "
      , ts ~ CollVal sts
      , KnownNat (ListMaxAlignment 0 ts)
      , KnownNat (Last (Acc0 0 ts ts))
      , T2L (Init (Acc0 0 ts ts))
      , PeekStruct ts
      )
  => Struct ts -> MPtr (At (StructPtr s) (Insert s sts dm)) dm
newptr s sts st = NewPtr (Proxy @s) (Proxy @sts) st ireturn

peekptr
  :: ( CheckJust (Lookup s dm) s "Peek freed ptr: "
     , ts ~ CollVal (FromJust (Lookup s dm))
     , KnownNat (ListMaxAlignment 0 ts)
     , KnownNat (Last (Acc0 0 ts ts))
     , T2L (Init (Acc0 0 ts ts))
     , PeekStruct ts
     )
  => StructPtr s -> MPtr (At (Struct (CollVal (FromJust (Lookup s dm)))) dm) dm
peekptr vs = PeekPtr vs ireturn

peekptrf
  :: forall s dm ts val offset n sts
   . StructPtr s
  -> forall (sym :: Symbol)
    ->( CheckJust (Lookup s dm) s "Peek freed ptr: "
      , CheckField s sym sts
      , sts ~ FromJust (Lookup s dm)
      , ts ~ CollVal sts
      , n ~ LookupField sym 0 sts
      , val ~ Index n ts
      , offset ~ Index n (Init (Acc0 0 ts ts))
      , Storable val
      , KnownNat offset
      )
  => MPtr (At (Index n ts) dm) dm
peekptrf vps sym =
  PeekPtrField
    vps
    (Proxy @sym)
    (Proxy @offset)
    ireturn

pokeptrf
  :: forall s val dm sts ts sym val' newts newdm offset n
   . StructPtr s
  -> forall (sym0 :: Symbol)
    ->val
  -> ( CheckJust (Lookup s dm) s "Poke freed ptr: "
     , CheckField s sym0 sts
     , sts ~ (FromJust (Lookup s dm))
     , ts ~ CollVal sts
     , n ~ LookupField sym0 0 sts
     , (sym ':-> val') ~ Index n sts
     , Check val' val
     , newts ~ UpdateIndex n (sym ':-> val) sts
     , newdm ~ InsertOverwriting s newts dm
     , offset ~ Index n (Init (Acc0 0 ts ts))
     , Storable val
     , KnownNat offset
     )
  => MPtr (At () newdm) dm
pokeptrf vps sym0 val = PokePtrField vps (Proxy @sym0) (Proxy @offset) val (returnAt ())

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
  NewPtr _ _ (st :: Struct ts) cont -> do
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
