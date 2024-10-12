{-# LANGUAGE AllowAmbiguousTypes #-}
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
       , KnownNat (ListMaxAlignment 0 ts)
       , KnownNat (Last (Acc0 0 ts ts))
       , ReifyOffsets (Init (Acc0 0 ts ts))
       , PeekStruct ts
       )
    => Proxy (s :: Symbol)
    -> Proxy (ts :: [Symbol :-> Type])
    -> Struct ts
    -> (At (ValPtr s) (Insert s (VStruct ts) dm) ~> MPtr ia)
    -> MPtr ia dm
  PeekStruct
    :: ( CheckJust (Lookup s dm) s "Peek freed ptr: "
       , ts ~ (FromStruct (FromJust (Lookup s dm)))
       , KnownNat (ListMaxAlignment 0 ts)
       , KnownNat (Last (Acc0 0 ts ts))
       , ReifyOffsets (Init (Acc0 0 ts ts))
       , PeekStruct ts
       )
    => ValPtr s
    -> (At (Struct ts) dm ~> MPtr ia)
    -> MPtr ia dm
  PeekPtr
    :: ( (hs ': syms) ~ SplitSymbol s
       , CheckJust (Lookup hs dm) hs "Peek freed ptr: "
       , ts ~ (FromStruct (FromJust (Lookup hs dm)))
       , '(offset, val) ~ FieldIndex syms '(0, Struct ts)
       , KnownNat offset
       , Storable val
       )
    => ValPtr s
    -> Proxy offset
    -> (At val dm ~> MPtr ia)
    -> MPtr ia dm
  PokePtr
    :: ( (hs ': syms) ~ SplitSymbol s
       , CheckJust (Lookup hs dm) hs "Poke freed ptr: "
       , VStruct ts ~ FromJust (Lookup hs dm)
       , sts ~ Struct ts
       , '(offset, val') ~ FieldIndex syms '(0, sts)
       , KnownNat offset
       , Check val' val
       , Struct newts ~ UpdateField syms '[] sts val
       , newdm ~ InsertOverwriting hs (VStruct newts) dm
       , Storable val
       )
    => ValPtr s
    -> Proxy (offset :: Nat)
    -> val
    -> MPtr ia newdm
    -> MPtr ia dm
  PeekStructField
    :: ( CheckJust (Lookup s dm) s "Peek freed ptr: "
       , ts ~ FromStruct (FromJust (Lookup s dm))
       , CheckField s sym ts
       , n ~ LookupField sym 0 ts
       , (anySym ':-> val) ~ Index n ts
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
       , ts ~ FromStruct (FromJust (Lookup s dm))
       , CheckField s sym ts
       , n ~ LookupField sym 0 ts
       , (anySym ':-> val') ~ Index n ts
       , Check val' val
       , newts ~ UpdateIndex n (anySym ':-> val) ts
       , newdm ~ InsertOverwriting s (VStruct newts) dm
       , offset ~ Index n (Init (Acc0 0 ts ts))
       , Storable val
       , KnownNat offset
       )
    => ValPtr s
    -> Proxy (sym :: Symbol)
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
    PeekPtr vs proxy contF -> PeekPtr vs proxy (imap f . contF)
    PokePtr vs proxy val cont -> PokePtr vs proxy val (imap f cont)
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
    PeekPtr vs proxy contF -> PeekPtr vs proxy (ibind f . contF)
    PokePtr vs proxy val cont -> PokePtr vs proxy val (ibind f cont)
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
    ->forall (ts :: [Symbol :-> Type])
    ->( CheckNothing (Lookup s dm) s "Already existing ptr: "
      , KnownNat (ListMaxAlignment 0 ts)
      , KnownNat (Last (Acc0 0 ts ts))
      , ReifyOffsets (Init (Acc0 0 ts ts))
      , PeekStruct ts
      )
  => Struct ts -> MPtr (At (ValPtr s) (Insert s (VStruct ts) dm)) dm
newStructPtr s ts st = NewStructPtr (Proxy @s) (Proxy @ts) st ireturn

peekStruct
  :: ( CheckJust (Lookup s dm) s "Peek freed ptr: "
     , ts ~ (FromStruct (FromJust (Lookup s dm)))
     , KnownNat (ListMaxAlignment 0 ts)
     , KnownNat (Last (Acc0 0 ts ts))
     , ReifyOffsets (Init (Acc0 0 ts ts))
     , PeekStruct ts
     )
  => ValPtr s -> MPtr (At (Struct ((FromStruct (FromJust (Lookup s dm))))) dm) dm
peekStruct vs = PeekStruct vs ireturn

peekptr
  :: forall offset s hs dm syms ts val
   . ( (hs ': syms) ~ SplitSymbol s
     , CheckJust (Lookup hs dm) hs "Peek freed ptr: "
     , ts ~ (FromStruct (FromJust (Lookup hs dm)))
     , '(offset, val) ~ FieldIndex syms '(0, Struct ts)
     , KnownNat offset
     , Storable val
     )
  => ValPtr s -> MPtr (At val dm) dm
peekptr vps = PeekPtr vps (Proxy @offset) ireturn

pokeptr
  :: forall hs syms s ts sts val' offset val newts newdm dm
   . ( (hs ': syms) ~ SplitSymbol s
     , CheckJust (Lookup hs dm) hs "Poke freed ptr: "
     , VStruct ts ~ FromJust (Lookup hs dm)
     , sts ~ Struct ts
     , '(offset, val') ~ FieldIndex syms '(0, sts)
     , KnownNat offset
     , Check val' val
     , Struct newts ~ UpdateField syms '[] sts val
     , newdm ~ InsertOverwriting hs (VStruct newts) dm
     , Storable val
     )
  => ValPtr s
  -> val
  -> MPtr (At () newdm) dm
pokeptr vps val = PokePtr vps (Proxy @offset) val (returnAt ())

peekStructf
  :: forall s dm ts val offset n anySym
   . ValPtr s
  -> forall (sym :: Symbol)
    ->( CheckJust (Lookup s dm) s "Peek freed ptr: "
      , CheckField s sym ts
      , ts ~ FromStruct (FromJust (Lookup s dm))
      , n ~ LookupField sym 0 ts
      , (anySym ':-> val) ~ Index n ts
      , offset ~ Index n (Init (Acc0 0 ts ts))
      , Storable val
      , KnownNat offset
      )
  => MPtr (At val dm) dm
peekStructf vps sym =
  PeekStructField
    vps
    (Proxy @sym)
    (Proxy @offset)
    ireturn

pokeStructf
  :: forall s val dm ts anySym val' newts newdm offset n
   . ValPtr s
  -> forall (sym :: Symbol)
    ->val
  -> ( CheckJust (Lookup s dm) s "Poke freed ptr: "
     , ts ~ FromStruct (FromJust (Lookup s dm))
     , CheckField s sym ts
     , n ~ LookupField sym 0 ts
     , (anySym ':-> val') ~ Index n ts
     , Check val' val
     , newts ~ UpdateIndex n (anySym ':-> val) ts
     , newdm ~ InsertOverwriting s (VStruct newts) dm
     , offset ~ Index n (Init (Acc0 0 ts ts))
     , Storable val
     , KnownNat offset
     )
  => MPtr (At () newdm) dm
pokeStructf vps sym val = PokeStructField vps (Proxy @sym) (Proxy @offset) val (returnAt ())

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
     , ts ~ FromStruct (FromJust (Lookup s dm))
     )
  => ValPtr s
  -> MPtr (At (Ptr (Struct ts)) dm) dm
toPtrStruct (ValPtrC ptr) = LiftM $ pure (returnAt (castPtr ptr))

toPtrStructField
  :: forall t s dm n offset ts anySym
   . (KnownNat offset)
  => ValPtr s
  -> forall (field :: Symbol)
    ->( CheckJust (Lookup s dm) s "Use freed ptr: "
      , ts ~ FromStruct (FromJust (Lookup s dm))
      , n ~ LookupField field 0 ts
      , (anySym ':-> t) ~ Index n ts
      , offset ~ Index n (Init (Acc0 0 ts ts))
      )
  => MPtr (At (Ptr t) dm) dm
toPtrStructField (ValPtrC ptr) _field = LiftM $ do
  let offsetVal = fromIntegral $ natVal (Proxy @offset)
  pure (returnAt (castPtr ptr `plusPtr` offsetVal))

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
  PeekPtr (ValPtrC ptr) (Proxy :: Proxy offset) (cont :: At val dma ~> MPtr ia) -> do
    let offset = fromIntegral $ natVal (Proxy @offset)
    val <- peek @val (castPtr (ptr `plusPtr` offset))
    runMPtr (cont (At val))
  PokePtr (ValPtrC ptr) (Proxy :: Proxy offset) val0 (cont) -> do
    let offset = fromIntegral $ natVal (Proxy @offset)
    poke (castPtr (ptr `plusPtr` offset)) val0
    runMPtr cont
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
