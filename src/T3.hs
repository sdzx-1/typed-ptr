{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
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
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module T3 where

import Data.IFunctor (At (..), IFunctor (..), IMonad (..), IMonadFail (..), returnAt, type (~>))
import qualified Data.IFunctor as I
import Data.Kind
import Data.Proxy
import Data.Type.Map (Insert, InsertOverwriting, Lookup, Map)
import Data.Void
import Foreign
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

data KVal = KType Type | KKey (Maybe Symbol)

type DM = Map Symbol [KVal]

data MMP (s :: Symbol) = forall a. MMP (Ptr a)

instance Show (MMP s) where
  show (MMP ptr) = show ptr

data MyVal (s :: KVal) where
  MyVal :: (Storable a, Show a) => a -> MyVal (KType a)
  MyNullPtr :: MyVal (KKey Nothing)
  MyMMP :: MMP s -> MyVal (KKey (Just s))

instance Show (MyVal s) where
  show = \case
    MyVal a -> show a
    MyNullPtr -> "NullPtr"
    MyMMP mmp -> show mmp

data Struct :: [KVal] -> Type where
  End :: Struct '[]
  Cons :: MyVal a -> Struct as -> Struct (a ': as)

type family Size a :: Nat

type instance Size Double = 8
type instance Size Int = 8

type family SizeOf (kvs :: [KVal]) :: Nat where
  SizeOf '[] = 0
  SizeOf ((KType x) ': xs) = Size x + SizeOf xs
  SizeOf (_ ': xs) = 8 + SizeOf xs

newPtrStruct :: forall vs. (KnownNat (SizeOf vs)) => Struct vs -> IO (Ptr (Struct vs))
newPtrStruct st = do
  let val = natVal (Proxy @(SizeOf vs))
  ptr <- mallocBytes (fromIntegral val)
  let go :: Int -> Struct ls -> IO ()
      go i str = case str of
        End -> pure ()
        Cons x xs -> do
          case x of
            MyVal a -> do
              poke (ptr `plusPtr` (i * 8)) a
              go (i + 1) xs
            MyMMP (MMP mp) -> do
              poke (ptr `plusPtr` (i * 8)) mp
              go (i + 1) xs
            MyNullPtr -> go (i + 1) xs
  go 0 st
  pure ptr

data MKey (ia :: DM -> Type) (b :: DM) where
  MReturn :: ia c -> MKey ia c
  NewKey
    :: (KnownNat (SizeOf vs))
    => Proxy (k :: Symbol)
    -> Struct vs
    -> (At (MMP k) (Insert k vs dm) ~> MKey ia)
    -> MKey ia dm
  LiftM :: IO (MKey ia dm') -> MKey ia dm

instance IFunctor MKey where
  imap f = \case
    MReturn ia -> MReturn (f ia)
    NewKey sym st cont -> NewKey sym st (imap f . cont)
    LiftM imk -> LiftM (fmap (imap f) imk)

instance IMonad MKey where
  ireturn = MReturn
  ibind f = \case
    MReturn ia -> f ia
    NewKey sym st cont -> NewKey sym st (ibind f . cont)
    LiftM imk -> LiftM (fmap (ibind f) imk)

instance IMonadFail MKey where
  fail = error "np"

runMKey :: MKey (At a dmi) dms -> IO a
runMKey = \case
  MReturn (At a) -> pure a
  NewKey _ st cont -> do
    ptr <- newPtrStruct st
    runMKey (cont (At (MMP ptr)))
  LiftM im -> do
    cont <- im
    runMKey cont

type family Index (i :: Nat) (vs :: [KVal]) :: KVal where
  Index 0 '[] = TypeError (Text "Index error, too big!")
  Index 0 (x ': _) = x
  Index n (_ ': xs) = Index (n - 1) xs

type family PeekVal (v :: (KVal)) :: (Type, Nat) where
  PeekVal (KKey _) = '(Ptr Void, 0)
  PeekVal (KType a) = '(a, 1)

newkey
  :: forall (k :: Symbol)
    ->(KnownNat (SizeOf vs))
  => Struct vs
  -> MKey (At (MMP k) (Insert k vs dm)) dm
newkey k st = NewKey (Proxy @k) st ireturn

liftm :: IO a -> MKey (At a dm) dm
liftm io = LiftM (io >>= \a -> pure (MReturn $ At a))

readkey
  :: forall n idx vals val s dm
   . ( Just vals ~ Lookup s dm
     , KnownNat n
     , KnownNat idx
     , Show val
     , '(val, idx) ~ PeekVal (Index n vals)
     , Storable val
     )
  => MMP s -> Proxy n -> MKey (At (MyVal (Index n vals)) dm) dm
readkey (MMP ptr) _ = LiftM $ do
  let nval = fromIntegral $ natVal @n Proxy
  v1 <- peek @val (castPtr (ptr `plusPtr` (nval * 8)))
  case natVal @idx Proxy of
    0 -> pure $ returnAt $ unsafeCoerce $ (MyMMP $ MMP $ unsafeCoerce v1)
    1 -> pure $ returnAt $ unsafeCoerce $ (MyVal v1)
    _ -> error "np"

type family UpdateIndex (s :: Symbol) (n :: Nat) (vals :: [KVal]) :: [KVal] where
  UpdateIndex s 0 (x ': xs) = KKey (Just s) ': xs
  UpdateIndex s n (x ': xs) = x ': UpdateIndex s (n - 1) xs

type family PokeVal (val' :: KVal) (val :: KVal) (n :: Nat) (vals :: [KVal]) :: (Type, [KVal]) where
  PokeVal (KType a) (KType a) n vals = '(a, vals)
  PokeVal (KKey _) (KKey (Just s)) n vals = '(Ptr Void, UpdateIndex s n vals)

writekeyField
  :: forall n s dm val' decVal val vals vals'
   . ( Just vals ~ Lookup s dm
     , val' ~ Index n vals
     , '(decVal, vals') ~ PokeVal val' val n vals
     , Storable decVal
     , KnownNat n
     )
  => MMP s -> Proxy n -> MyVal val -> MKey (At () (InsertOverwriting s vals' dm)) dm
writekeyField (MMP ptr) _ myVal = LiftM $ do
  let nval = natVal @n Proxy
  case myVal of
    MyVal a -> do
      poke @decVal (castPtr (ptr `plusPtr` (fromIntegral nval * 8))) (unsafeCoerce a)
      pure (returnAt ())
    MyMMP (MMP ptr) -> do
      poke (castPtr (ptr `plusPtr` (fromIntegral nval * 8))) ptr
      pure (returnAt ())
    _ -> pure (returnAt ())

tt :: MKey (At () '[]) '[]
tt = I.do
  At k1 <-
    newkey
      "k1"
      ( Cons
          (MyVal (100 :: Double))
          ( Cons
              (MyVal (10087 :: Double))
              (Cons MyNullPtr End)
          )
      )

  At k2 <-
    newkey
      "k2"
      ( ( Cons
            (MyVal (3.14 :: Double))
            (Cons MyNullPtr End)
        )
      )

  liftm $ print (("k1", k1), ("k2", k2))
  writekeyField k1 (Proxy @0) $ MyVal @Double 99
  writekeyField k1 (Proxy @2) $ MyMMP k2
  writekeyField k2 (Proxy @1) $ MyMMP k2

  At mv <- readkey k1 (Proxy @0)
  liftm $ print mv
  At mv <- readkey k1 (Proxy @1)
  liftm $ print mv
  At mv <- readkey k1 (Proxy @2)
  liftm $ print mv
  At mv <- readkey k2 (Proxy @0)
  liftm $ print mv
  At mv <- readkey k2 (Proxy @1)
  liftm $ print mv

  At (MyMMP mmp ) <- readkey k2 (Proxy @1)

  writekeyField mmp (Proxy @0) (MyVal @Double 4.13)
  At mv <- readkey k2 (Proxy @0)
  liftm $ print mv
  undefined

runtt = runMKey tt
