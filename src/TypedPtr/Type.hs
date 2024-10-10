{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TypedPtr.Type where

import Data.Foldable (for_)
import Data.Kind
import Data.Proxy
import Data.Traversable (for)
import Data.Type.Map (Map, type (:->) (..))
import Foreign
import GHC.TypeError (Unsatisfiable)
import GHC.TypeLits
import TypedPtr.Storable

type DM = Map Symbol [Symbol :-> Type]

data NullPtr = NullPtrC

type instance Alignment NullPtr = 8
type instance Size NullPtr = 8

instance Show NullPtr where
  show _ = "NullPtr"

instance Storable NullPtr where
  sizeOf _ = 8
  alignment _ = 8
  peek _ = pure NullPtrC
  poke _ _ = pure ()

data Array (size :: Nat) (a :: Type) = ArrayC [a]

type instance
  Alignment (Array size t) =
    Alignment t
type instance Size (Array size t) = size * Size t

instance (KnownNat n, Show a) => Show (Array n a) where
  show (ArrayC ls) = "Array (size " <> show (natVal (Proxy @n)) <> "): " <> show ls

instance (Eq a) => Eq (Array n a) where
  ArrayC xs == ArrayC ys = xs == ys

instance
  ( Storable t
  , KnownNat size
  )
  => Storable (Array size t)
  where
  sizeOf _ = fromIntegral (natVal (Proxy @size)) * (sizeOf @t undefined)
  alignment _ = alignment @t undefined
  peek ptr = do
    let len = fromIntegral $ natVal (Proxy @size)
    vals <- for [0 .. len - 1] $
      \i -> peek @t (castPtr (ptr `plusPtr` (i * (sizeOf @t undefined))))
    pure (ArrayC vals)
  poke ptr (ArrayC ls) = do
    let len = fromIntegral $ natVal (Proxy @size)
    for_ (zip [0 .. len - 1] (take len ls)) $
      \(i, v) -> poke @t (castPtr (ptr `plusPtr` (i * (sizeOf @t undefined)))) v

data ValPtr (s :: Symbol) = forall a. ValPtrC (Ptr a)

type instance Alignment (ValPtr s) = 8
type instance Size (ValPtr s) = 8

instance Show (ValPtr s) where
  show (ValPtrC ptr) = show ptr

instance Storable (ValPtr a) where
  sizeOf _ = 8
  alignment _ = 8
  peek ptr = do
    val <- peek (castPtr ptr)
    pure (ValPtrC val)
  poke ptr (ValPtrC nptr) = poke (castPtr ptr) nptr

type instance Alignment (Maybe a) = Alignment a
type instance Size (Maybe a) = Size a + 1

instance (Storable a) => Storable (Maybe a) where
  sizeOf _ = sizeOf @a undefined + 1
  alignment _ = alignment @a undefined
  peek ptr = do
    filled <- peekByteOff ptr $ sizeOf @a undefined
    if filled == (1 :: Word8)
      then Just <$> peek @a (castPtr ptr)
      else return Nothing
  poke ptr Nothing = pokeByteOff ptr (sizeOf @a undefined) (0 :: Word8)
  poke ptr (Just a) = do
    poke (castPtr ptr) a
    pokeByteOff ptr (sizeOf @a undefined) (1 :: Word8)

infixr 4 :&

data Struct :: [Type] -> Type where
  End :: Struct '[]
  (:&) :: (Storable a) => a -> Struct as -> Struct (a ': as)

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

instance (All Show ts) => Show (Struct ts) where
  show = \case
    End -> "{}"
    v :& End -> "{" ++ show v ++ "}"
    v :& vs -> "{" ++ show v ++ ", " ++ (drop 1 $ show vs)

instance (All Eq ts) => Eq (Struct ts) where
  End == End = True
  (x :& xs) == (y :& ys) = x == y && xs == ys

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
  , ReifyOffsets (Init (Acc0 0 ts ts))
  , PeekStruct ts
  )
  => Storable (Struct ts)
  where
  sizeOf _ = fromIntegral $ natVal (Proxy @(Size (Struct ts)))
  alignment _ = fromIntegral $ natVal (Proxy @(Alignment (Struct ts)))
  poke ptr struct = do
    let offsets = reifyOffsets (Proxy @(Init (Acc0 0 ts ts)))
    pokeStruct ptr offsets struct
  peek ptr = do
    let offsets = reifyOffsets (Proxy @(Init (Acc0 0 ts ts)))
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

type family UpdateIndex (n :: Nat) (v :: a) (ts :: [a]) :: [a] where
  UpdateIndex _ _ '[] = TypeError (Text "Too big index")
  UpdateIndex 0 a (x ': xs) = a ': xs
  UpdateIndex n a (x ': xs) = x ': UpdateIndex (n - 1) a xs

type family Check (val' :: Type) (val :: Type) :: Constraint where
  Check NullPtr NullPtr = ()
  Check NullPtr (ValPtr s) = ()
  Check (ValPtr s) NullPtr = ()
  Check (ValPtr s) (ValPtr s) = ()
  Check (ValPtr s) (ValPtr s1) = ()
  Check a a = ()
  Check a b =
    Unsatisfiable
      ( Text "Poke the error type, E: "
          :<>: ShowType a
          :<>: Text " A: "
          :<>: ShowType b
      )

type family CollVal (ls :: [Symbol :-> Type]) :: [Type] where
  CollVal '[] = '[]
  CollVal ((_ ':-> v) ': xs) = v ': CollVal xs

type family CheckField (ptr :: Symbol) (sym :: Symbol) (sts :: [Symbol :-> Type]) :: Constraint where
  CheckField ptr sym '[] =
    Unsatisfiable (Text "ptr: " :<>: ShowType ptr :<>: Text " not have field: " :<>: ShowType sym)
  CheckField ptr sym ((sym ':-> _) ': xs) = ()
  CheckField ptr sym ((_ ':-> _) ': xs) = CheckField ptr sym xs

type family LookupField (sym :: Symbol) (i :: Nat) (sts :: [Symbol :-> Type]) :: Nat where
  LookupField sym i ((sym ':-> _) ': xs) = i
  LookupField sym i ((_ ':-> _) ': xs) = LookupField sym (i + 1) xs

type family DeleteList (v :: Type) (sls :: [Symbol :-> Type]) :: [Symbol :-> Type] where
  DeleteList (ValPtr s) ((sym ':-> ValPtr s) ': xs) =
    (sym ':-> NullPtr) ': DeleteList (ValPtr s) xs
  DeleteList (ValPtr s) (x ': xs) = x ': DeleteList (ValPtr s) xs
  DeleteList (ValPtr s) '[] = '[]

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
