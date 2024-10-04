{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module T2 where

import Data.IFunctor (At (..), IFunctor (..), IMonad (..), returnAt, type (~>))
import qualified Data.IFunctor as I
import Data.Kind
import Data.Proxy
import Data.Type.Map
import Data.Void
import Foreign (Ptr, Storable (..), castPtr, plusPtr)
import Foreign.Marshal.Alloc
import GHC.Records
import GHC.TypeLits

data KVal = KType Type | KKey (Maybe Symbol)

data MyVal (s :: KVal) where
  MyVal :: a -> MyVal (KType a)
  MyNullVal :: MyVal (KKey Nothing)
  MyMMP :: MMP s -> MyVal (KKey (Just s))

data Struct :: [KVal] -> Type where
  End :: Struct '[]
  Cons :: MyVal a -> Struct as -> Struct (a ': as)

-- data PtrStruct (s :: [KVal]) = forall a. PtrStruct (Ptr a)

-- instance HasField "v0" (PtrStruct ((KType x) ': xs)) (Ptr x) where
--   getField (PtrStruct ptr) = castPtr ptr

-- instance HasField "v1" (PtrStruct (a ': (KType x) ': xs)) (Ptr x) where
--   getField (PtrStruct ptr) = castPtr (ptr `plusPtr` 8)

-- instance HasField "v2" (PtrStruct (a ': b ': (KType x) ': xs)) (Ptr x) where
--   getField (PtrStruct ptr) = castPtr (ptr `plusPtr` 16)

-- ks :: Struct [KType Double, KType Double]
-- ks = Cons (MyVal 1) (Cons (MyVal 2) End)

-- ps :: PtrStruct [KType Double, KType Double]
-- ps = undefined

{-

ptr -> [100, True, Sring, Just ptr1]
        0     1     2      3
ptr1 -> [3]

ptr.0 .= 20
ptr.1 .= False
-- (ptr %  3) % 0 .= 100
ref <- readKey ptr.3
ref .= 100

-}

-------------------------------

type DM = Map Symbol (Maybe Symbol)

data MMP (s :: Symbol) = MMP (Ptr (Ptr Void))

instance forall s. (KnownSymbol s) => Show (MMP s) where
  show (MMP ptr) =
    let sk = symbolVal (Proxy @s)
     in "Symbol: " <> sk <> ", " <> show ptr

type family DeleteMaybe (v :: Symbol) (ls :: Maybe Symbol) :: Maybe Symbol where
  DeleteMaybe v Nothing = Nothing
  DeleteMaybe v (Just v) = Nothing
  DeleteMaybe v (Just k) = Just k

type family DeleteVal (v :: Symbol) (i :: DM) :: DM where
  DeleteVal v '[] = '[]
  DeleteVal v ((k ':-> ls) ': is) = (k ':-> DeleteMaybe v ls) ': DeleteVal v is

data MKey (ia :: DM -> Type) (b :: DM) where
  MReturn :: ia c -> MKey ia c
  NewKey :: Proxy (s :: Symbol) -> (At (MMP s) (Insert s Nothing dm) ~> MKey ia) -> MKey ia dm
  UpdateKey :: MMP s -> MMP v -> MKey ia (InsertOverwriting s (Just v) dm) -> MKey ia dm
  FreeKey :: MMP s -> MKey ia (DeleteVal s (Delete s dm)) -> MKey ia dm
  LiftM :: IO (MKey ia dm') -> MKey ia dm

runMyKey :: MKey (At a dmi) dms -> IO a
runMyKey = \case
  MReturn (At a) -> pure a
  NewKey _ cont -> do
    ptr <- malloc @(Ptr Void)
    runMyKey (cont (At (MMP ptr)))
  UpdateKey (MMP pts) (MMP ptv) cont -> do
    poke (castPtr pts) ptv
    runMyKey cont
  FreeKey (MMP ptr) cont -> do
    free ptr
    runMyKey cont
  LiftM im -> do
    cont <- im
    runMyKey cont

instance IFunctor MKey where
  imap f = \case
    MReturn ia -> MReturn (f ia)
    NewKey st cont -> NewKey st (imap f . cont)
    UpdateKey s v cont -> UpdateKey s v (imap f cont)
    FreeKey ak cont -> FreeKey ak (imap f cont)
    LiftM imk -> LiftM (fmap (imap f) imk)

instance IMonad MKey where
  ireturn = MReturn
  ibind f = \case
    MReturn ia -> f ia
    NewKey st cont -> NewKey st (ibind f . cont)
    UpdateKey s v cont -> UpdateKey s v (ibind f cont)
    FreeKey ak cont -> FreeKey ak (ibind f cont)
    LiftM imk -> LiftM (fmap (ibind f) imk)

liftm :: IO a -> MKey (At () dm) dm
liftm io = LiftM (io >> pure (returnAt ()))

newkey :: forall (s :: Symbol) -> MKey (At (MMP s) (Insert s Nothing dm)) dm
newkey s = NewKey (Proxy @s) ireturn

updatekey :: MMP s -> MMP v -> MKey (At () (InsertOverwriting s (Just v) dm)) dm
updatekey s v = UpdateKey s v (returnAt ())

type family LookupI (s :: k) (i :: [k :-> Maybe v]) :: v where
  LookupI k '[] = TypeError (Text "can't get key: " :<>: ShowType k)
  LookupI k ((k ':-> Nothing) ': _) = TypeError (Text "can't get val of key: " :<>: ShowType k)
  LookupI k ((k ':-> Just a) ': _) = a
  LookupI k (_ ': b) = LookupI k b

readkey :: (k ~ (LookupI s dm)) => MMP s -> MKey (At (MMP k) dm) dm
readkey (MMP ptr) =
  LiftM $ do
    t <- peek ptr
    pure $ returnAt (MMP $ castPtr t)

freekey :: MMP s -> MKey (At () (DeleteVal s (Delete s dm))) dm
freekey mmp = FreeKey mmp (returnAt ())

tt :: MKey (At () '[]) '[]
tt = I.do
  At k1 <- newkey "k1"
  At k2 <- newkey "k2"
  liftm $ print (k1, k2)
  updatekey k1 k2
  updatekey k2 k1
  At k2v <- readkey k2
  liftm $ print k2v
  freekey k2
  freekey k1

runtt :: IO ()
runtt = runMyKey tt

foo :: Int -> forall (s :: [Symbol]) -> Int
foo _ _ss = undefined

kkk = foo 10 ["nice", "help", "gk"]
