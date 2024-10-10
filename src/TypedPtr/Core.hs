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
  NewSingletonPtr
    :: ( CheckNothing (Lookup s dm) s "Already existing ptr: "
       , Storable t
       )
    => Proxy (s :: Symbol)
    -> t
    -> (At (ValPtr s) (Insert s (VSingleton t) dm) ~> MPtr ia)
    -> MPtr ia dm
  NewStructPtr
    :: ( CheckNothing (Lookup s dm) s "Already existing ptr: "
       , ts ~ CollVal sts
       , KnownNat (ListMaxAlignment 0 ts)
       , KnownNat (Last (Acc0 0 ts ts))
       , ReifyOffsets (Init (Acc0 0 ts ts))
       , PeekStruct ts
       )
    => Proxy (s :: Symbol)
    -> Proxy (sts :: [Symbol :-> Type])
    -> Struct ts
    -> (At (ValPtr s) (Insert s (VStruct sts) dm) ~> MPtr ia)
    -> MPtr ia dm
  PeekStruct
    :: ( CheckJust (Lookup s dm) s "Peek freed ptr: "
       , ts ~ CollVal (FromStruct (FromJust (Lookup s dm)))
       , KnownNat (ListMaxAlignment 0 ts)
       , KnownNat (Last (Acc0 0 ts ts))
       , ReifyOffsets (Init (Acc0 0 ts ts))
       , PeekStruct ts
       )
    => ValPtr s
    -> (At (Struct ts) dm ~> MPtr ia)
    -> MPtr ia dm
  PeekStructField
    :: ( CheckJust (Lookup s dm) s "Peek freed ptr: "
       , sts ~ FromStruct (FromJust (Lookup s dm))
       , CheckField s sym sts
       , ts ~ CollVal sts
       , n ~ LookupField sym 0 sts
       , val ~ Index n ts
       , offset ~ Index n (Init (Acc0 0 ts ts))
       , Storable val
       , KnownNat offset
       )
    => ValPtr s
    -> Proxy (sym :: Symbol)
    -> Proxy (offset :: Nat)
    -> (At val dm ~> MPtr ia)
    -> MPtr ia dm
  PokeStructField
    :: ( CheckJust (Lookup s dm) s "Poke freed ptr: "
       , sts ~ FromStruct (FromJust (Lookup s dm))
       , CheckField s sym0 sts
       , ts ~ CollVal sts
       , n ~ LookupField sym0 0 sts
       , (sym ':-> val') ~ Index n sts
       , Check val' val
       , newts ~ UpdateIndex n (sym ':-> val) sts
       , newdm ~ InsertOverwriting s (VStruct newts) dm
       , offset ~ Index n (Init (Acc0 0 ts ts))
       , Storable val
       , KnownNat offset
       )
    => ValPtr s
    -> Proxy (sym0 :: Symbol)
    -> Proxy (offset :: Nat)
    -> val
    -> MPtr ia newdm
    -> MPtr ia dm
  FreePtr
    :: ( CheckJust (Lookup s dm) s ("Double free ptr: ")
       , newdm ~ DeleteVal (ValPtr s) (Delete s dm)
       )
    => ValPtr s
    -> MPtr ia newdm
    -> MPtr ia dm
  LiftM :: IO (MPtr ia dm) -> MPtr ia dm

instance IFunctor MPtr where
  imap f = \case
    MReturn a -> MReturn (f a)
    NewSingletonPtr s t cont -> NewSingletonPtr s t (imap f . cont)
    NewStructPtr s sts st contF -> NewStructPtr s sts st (imap f . contF)
    PeekStruct vs contF -> PeekStruct vs (imap f . contF)
    PeekStructField vs n offset contF -> PeekStructField vs n offset (imap f . contF)
    PokeStructField vs n offset val cont -> PokeStructField vs n offset val (imap f cont)
    FreePtr vs cont -> FreePtr vs (imap f cont)
    LiftM ma -> LiftM (fmap (imap f) ma)

instance IMonad MPtr where
  ireturn = MReturn
  ibind f = \case
    MReturn a -> f a
    NewSingletonPtr s t cont -> NewSingletonPtr s t (ibind f . cont)
    NewStructPtr s sts st contF -> NewStructPtr s sts st (ibind f . contF)
    PeekStruct vs contF -> PeekStruct vs (ibind f . contF)
    PeekStructField vs n offset contF -> PeekStructField vs n offset (ibind f . contF)
    PokeStructField vs n offset val cont -> PokeStructField vs n offset val (ibind f cont)
    FreePtr vs cont -> FreePtr vs (ibind f cont)
    LiftM ma -> LiftM (fmap (ibind f) ma)

newSingletonPtr
  :: forall (s :: Symbol)
    ->( CheckNothing (Lookup s dm) s "Already existing ptr: "
      , Storable t
      )
  => t -> MPtr (At (ValPtr s) (Insert s (VSingleton t) dm)) dm
newSingletonPtr s t = NewSingletonPtr (Proxy @s) t ireturn

newStructPtr
  :: forall (s :: Symbol)
    ->forall (sts :: [Symbol :-> Type])
    ->( CheckNothing (Lookup s dm) s "Already existing ptr: "
      , ts ~ CollVal sts
      , KnownNat (ListMaxAlignment 0 ts)
      , KnownNat (Last (Acc0 0 ts ts))
      , ReifyOffsets (Init (Acc0 0 ts ts))
      , PeekStruct ts
      )
  => Struct ts -> MPtr (At (ValPtr s) (Insert s (VStruct sts) dm)) dm
newStructPtr s sts st = NewStructPtr (Proxy @s) (Proxy @sts) st ireturn

peekStruct
  :: ( CheckJust (Lookup s dm) s "Peek freed ptr: "
     , ts ~ CollVal (FromStruct (FromJust (Lookup s dm)))
     , KnownNat (ListMaxAlignment 0 ts)
     , KnownNat (Last (Acc0 0 ts ts))
     , ReifyOffsets (Init (Acc0 0 ts ts))
     , PeekStruct ts
     )
  => ValPtr s -> MPtr (At (Struct (CollVal (FromStruct (FromJust (Lookup s dm))))) dm) dm
peekStruct vs = PeekStruct vs ireturn

peekStructf
  :: forall s dm ts val offset n sts
   . ValPtr s
  -> forall (sym :: Symbol)
    ->( CheckJust (Lookup s dm) s "Peek freed ptr: "
      , CheckField s sym sts
      , sts ~ FromStruct (FromJust (Lookup s dm))
      , ts ~ CollVal sts
      , n ~ LookupField sym 0 sts
      , val ~ Index n ts
      , offset ~ Index n (Init (Acc0 0 ts ts))
      , Storable val
      , KnownNat offset
      )
  => MPtr (At (Index n ts) dm) dm
peekStructf vps sym =
  PeekStructField
    vps
    (Proxy @sym)
    (Proxy @offset)
    ireturn

pokeStructf
  :: forall s val dm sts ts sym val' newts newdm offset n
   . ValPtr s
  -> forall (sym0 :: Symbol)
    ->val
  -> ( CheckJust (Lookup s dm) s "Poke freed ptr: "
     , CheckField s sym0 sts
     , sts ~ FromStruct (FromJust (Lookup s dm))
     , ts ~ CollVal sts
     , n ~ LookupField sym0 0 sts
     , (sym ':-> val') ~ Index n sts
     , Check val' val
     , newts ~ UpdateIndex n (sym ':-> val) sts
     , newdm ~ InsertOverwriting s (VStruct newts) dm
     , offset ~ Index n (Init (Acc0 0 ts ts))
     , Storable val
     , KnownNat offset
     )
  => MPtr (At () newdm) dm
pokeStructf vps sym0 val = PokeStructField vps (Proxy @sym0) (Proxy @offset) val (returnAt ())

freeptr
  :: ( CheckJust (Lookup s dm) s ("Double free ptr: ")
     , newdm ~ DeleteVal (ValPtr s) (Delete s dm)
     )
  => ValPtr s
  -> MPtr (At () (DeleteVal (ValPtr s) (Delete s dm))) dm
freeptr vs = FreePtr vs (returnAt ())

liftm :: IO a -> MPtr (At a dm) dm
liftm ma = LiftM (ma >>= pure . returnAt)

toPtrStruct
  :: ( CheckJust (Lookup s dm) s "Use freed ptr: "
     , sts ~ FromStruct (FromJust (Lookup s dm))
     )
  => ValPtr s
  -> MPtr (At (Ptr (Struct (CollVal sts))) dm) dm
toPtrStruct (ValPtrC ptr) = LiftM $ pure (returnAt (castPtr ptr))

runMPtr :: MPtr (At a dm') dm -> IO a
runMPtr = \case
  MReturn (At a) -> pure a
  NewSingletonPtr _ a cont -> do
    ptr <- mallocBytes (sizeOf a)
    runMPtr (cont (At (ValPtrC ptr)))
  NewStructPtr _ _ (st :: Struct ts) cont -> do
    ptr <- malloc @(Struct ts)
    poke ptr st
    runMPtr (cont (At (ValPtrC ptr)))
  PeekStruct (ValPtrC ptr) (cont :: At (Struct ts) dma ~> MPtr (At a dmb)) -> do
    val <- peek @(Struct ts) (castPtr ptr)
    runMPtr (cont (At val))
  PeekStructField
    (ValPtrC ptr)
    (Proxy :: Proxy n)
    (Proxy :: Proxy offset)
    (cont :: At val dma ~> MPtr ia) -> do
      let offset = fromIntegral $ natVal (Proxy @offset)
      val <- peek @val (castPtr (ptr `plusPtr` offset))
      runMPtr (cont (At val))
  PokeStructField
    (ValPtrC ptr)
    (Proxy :: Proxy n)
    (Proxy :: Proxy offset)
    (val :: val)
    cont -> do
      let offset = fromIntegral $ natVal (Proxy @offset)
      poke @val (castPtr (ptr `plusPtr` offset)) val
      runMPtr cont
  FreePtr (ValPtrC ptr) cont -> do
    free ptr
    runMPtr cont
  LiftM mcont -> do
    m <- mcont
    runMPtr m
