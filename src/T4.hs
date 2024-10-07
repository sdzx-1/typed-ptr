{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module T4 where

import Data.IFunctor (At (..), IFunctor (..), IMonad (..), returnAt, type (~>))
import Data.Kind
import Data.Proxy
import Data.Type.Map (Delete, Insert, InsertOverwriting, Lookup, Map, type (:->) (..))
import Foreign
import GHC.TypeError (Unsatisfiable)
import GHC.TypeLits
import S1

type DM = Map Symbol [Type]

data NullPtr = NullPtrC
instance Show NullPtr where
  show _ = "NullPtr"

type instance Alignment NullPtr = 8
type instance Size NullPtr = 8

data StructPtr (s :: Symbol) = forall a. StructPtrC (Ptr a)

type instance Alignment (StructPtr s) = 8
type instance Size (StructPtr s) = 8

instance Show (StructPtr s) where
  show (StructPtrC ptr) = show ptr

instance Storable NullPtr where
  sizeOf _ = 8
  alignment _ = 8
  peek _ = pure NullPtrC
  poke _ _ = pure ()

instance Storable (StructPtr a) where
  sizeOf _ = 8
  alignment _ = 8
  peek ptr = do
    val <- peek (castPtr ptr)
    pure (StructPtrC val)
  poke ptr (StructPtrC nptr) = poke (castPtr ptr) nptr

infixr 4 :&

data Struct :: [Type] -> Type where
  End :: Struct '[]
  (:&) :: (Storable a) => a -> Struct as -> Struct (a ': as)

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

instance (All Show ts) => Show (Struct ts) where
  show = \case
    End -> "}}"
    v :& End -> "{" ++ show v ++ "}"
    v :& vs -> "{" ++ show v ++ ", " ++ (drop 1 $ show vs)

type instance Alignment (Struct xs) = ListMaxAlignment 0 xs
type instance Size (Struct xs) = Last (Acc0 0 xs xs)

pokeStruct :: Ptr a -> [Int] -> Struct ts -> IO ()
pokeStruct _ [] End = pure ()
pokeStruct ptr (offset : offsets) (x :& xs) = do
  poke (castPtr (ptr `plusPtr` offset)) x
  pokeStruct ptr offsets xs
pokeStruct _ _ _ = error "np"

class PeekStruct ts where
  peekStruct :: [Int] -> Ptr a -> IO (Struct ts)

instance PeekStruct '[] where
  peekStruct [] _ptr = pure End
  peekStruct _ _ = error "np"

instance (Storable x, PeekStruct xs) => PeekStruct (x ': xs) where
  peekStruct [] _ = error "np"
  peekStruct (offset : offsets) ptr = do
    v <- peek @x (ptr `plusPtr` offset)
    vs <- peekStruct offsets ptr
    pure (v :& vs)

instance
  ( KnownNat (ListMaxAlignment 0 ts)
  , KnownNat (Last (Acc0 0 ts ts))
  , T2L (Init (Acc0 0 ts ts))
  , PeekStruct ts
  )
  => Storable (Struct ts)
  where
  sizeOf _ = fromIntegral $ natVal (Proxy @(Alignment (Struct ts)))
  alignment _ = fromIntegral $ natVal (Proxy @(Size (Struct ts)))
  poke ptr struct = do
    let offsets = t2l (Proxy @(Init (Acc0 0 ts ts)))
    pokeStruct ptr offsets struct
  peek ptr = do
    let offsets = t2l (Proxy @(Init (Acc0 0 ts ts)))
    peekStruct offsets ptr

type family
  Index
    (n :: Nat)
    (ts :: [a])
    :: a
  where
  Index _ '[] = TypeError (Text "Too big index")
  Index 0 (x ': _) = x
  Index n (x ': xs) = Index (n - 1) xs

type family UpdateIndex (n :: Nat) (v :: Type) (ts :: [Type]) :: [Type] where
  UpdateIndex _ _ '[] = TypeError (Text "Too big index")
  UpdateIndex 0 a (x ': xs) = a ': xs
  UpdateIndex n a (x ': xs) = x ': UpdateIndex (n - 1) a xs

type family Check (val' :: Type) (val :: Type) :: Constraint where
  Check NullPtr NullPtr = ()
  Check NullPtr (StructPtr s) = ()
  Check (StructPtr s) NullPtr = ()
  Check (StructPtr s) (StructPtr s) = ()
  Check (StructPtr s) (StructPtr s1) = ()
  Check a a = ()
  Check a b =
    Unsatisfiable
      ( Text "Poke the error type, E: "
          :<>: ShowType a
          :<>: Text " A: "
          :<>: ShowType b
      )

type family DeleteList (v :: Type) (ls :: [Type]) :: [Type] where
  DeleteList (StructPtr s) (StructPtr s ': xs) = NullPtr ': DeleteList (StructPtr s) xs
  DeleteList (StructPtr s) (x ': xs) = x ': DeleteList (StructPtr s) xs
  DeleteList (StructPtr s) '[] = '[]

type family DeleteVal (v :: Type) (i :: DM) :: DM where
  DeleteVal v '[] = '[]
  DeleteVal v ((k ':-> ls) ': is) = (k ':-> DeleteList v ls) ': DeleteVal v is

type family CheckJust (ms :: Maybe a) (sym :: Symbol) (errorMsg :: Symbol) :: Constraint where
  CheckJust (Just _) _ _ = ()
  CheckJust Nothing sym errorMsg = Unsatisfiable (Text errorMsg :<>: ShowType sym)

type family CheckNothing (ms :: Maybe a) (sym :: Symbol) (errorMsg :: Symbol) :: Constraint where
  CheckNothing Nothing _ _ = ()
  CheckNothing (Just _) sym errorMsg = Unsatisfiable (Text errorMsg :<>: ShowType sym)

type family FromJust (s :: Maybe a) :: a where
  FromJust (Just a) = a

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
