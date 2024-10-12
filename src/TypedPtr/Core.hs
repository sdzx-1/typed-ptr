{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TypedPtr.Core where

import Data.IFunctor (At (..), IFunctor (..), IMonad (..), returnAt, type (~>))
import qualified Data.IFunctor as I
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
    PeekPtr vs proxy contF -> PeekPtr vs proxy (imap f . contF)
    PokePtr vs proxy val cont -> PokePtr vs proxy val (imap f cont)
    FreePtr vs cont -> FreePtr vs (imap f cont)
    LiftM ma -> LiftM (fmap (imap f) ma)

instance IMonad MPtr where
  ireturn = MReturn
  ibind f = \case
    MReturn a -> f a
    NewSingletonPtr s t cont -> NewSingletonPtr s t (ibind f . cont)
    NewStructPtr s sts st contF -> NewStructPtr s sts st (ibind f . contF)
    PeekPtr vs proxy contF -> PeekPtr vs proxy (ibind f . contF)
    PokePtr vs proxy val cont -> PokePtr vs proxy val (ibind f cont)
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

freeptr
  :: ( CheckJust (Lookup s dm) s ("Double free ptr: ")
     , newdm ~ DeleteVal (ValPtr s) (Delete s dm)
     )
  => ValPtr s
  -> MPtr (At () (DeleteVal (ValPtr s) (Delete s dm))) dm
freeptr vs = FreePtr vs (returnAt ())

liftm :: IO a -> MPtr (At a dm) dm
liftm ma = LiftM (ma >>= pure . returnAt)

ptrffi
  :: forall a hs syms s dm ts offset val
   . ( (hs ': syms) ~ SplitSymbol s
     , CheckJust (Lookup hs dm) hs "Use freed ptr: "
     , ts ~ (FromStruct (FromJust (Lookup hs dm)))
     , '(offset, val) ~ FieldIndex syms '(0, Struct ts)
     , KnownNat offset
     , Storable val
     )
  => ValPtr s
  -> (Ptr val -> IO a)
  -> MPtr (At a dm) dm
ptrffi (ValPtrC ptr) fun = LiftM $ do
  let offset = fromIntegral $ natVal (Proxy @offset)
  val <- fun (ptr `plusPtr` offset)
  pure (returnAt val)

copyStruct
  :: forall dest source dm hs syms ts offset val hs1 syms1 ts1 offset1 val1
   . ( (hs ': syms) ~ SplitSymbol dest
     , CheckJust (Lookup hs dm) hs "Use freed ptr: "
     , ts ~ (FromStruct (FromJust (Lookup hs dm)))
     , '(offset, val) ~ FieldIndex syms '(0, Struct ts)
     , KnownNat offset
     , Storable val
     , (hs1 ': syms1) ~ SplitSymbol source
     , CheckJust (Lookup hs1 dm) hs1 "Use freed ptr: "
     , ts1 ~ (FromStruct (FromJust (Lookup hs1 dm)))
     , '(offset1, val1) ~ FieldIndex syms1 '(0, Struct ts1)
     , KnownNat offset1
     , Storable val1
     , NotEq (CmpSymbol hs hs1)
     , val ~ val1
     )
  => ValPtr dest -> ValPtr source -> MPtr (At () dm) dm
copyStruct (ValPtrC destptr) (ValPtrC sourceptr) = LiftM $ do
  let offset = fromIntegral (natVal (Proxy @offset))
      offset1 = fromIntegral (natVal (Proxy @offset1))
  copyBytes (destptr `plusPtr` offset) (sourceptr `plusPtr` offset1) (sizeOf @val undefined)
  pure (returnAt ())

printptr
  :: ( (hs ': syms) ~ SplitSymbol s
     , CheckJust (Lookup hs dm) hs "Peek freed ptr: "
     , ts ~ (FromStruct (FromJust (Lookup hs dm)))
     , '(offset, val) ~ FieldIndex syms '(0, Struct ts)
     , KnownNat offset
     , Storable val
     , Show val
     )
  => String
  -> ValPtr s
  -> MPtr (At () dm) dm
printptr st vps = I.do
  At val <- peekptr vps
  liftm $ putStrLn (st ++ ": " ++ show val)

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
  PeekPtr (ValPtrC ptr) (Proxy :: Proxy offset) (cont :: At val dma ~> MPtr ia) -> do
    let offset = fromIntegral $ natVal (Proxy @offset)
    val <- peek @val (castPtr (ptr `plusPtr` offset))
    runMPtr (cont (At val))
  PokePtr (ValPtrC ptr) (Proxy :: Proxy offset) val0 (cont) -> do
    let offset = fromIntegral $ natVal (Proxy @offset)
    poke (castPtr (ptr `plusPtr` offset)) val0
    runMPtr cont
  FreePtr (ValPtrC ptr) cont -> do
    free ptr
    runMPtr cont
  LiftM mcont -> do
    m <- mcont
    runMPtr m
