{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module T4 where

import Data.IFunctor (At (..), IFunctor (..), IMonad (..), returnAt, type (~>))
import qualified Data.IFunctor as I
import Data.Kind
import Data.Proxy
import Data.Type.Map (Delete, Insert, InsertOverwriting, Lookup, Map, type (:->) (..))
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
  Index
    (n :: Nat)
    (ts :: [Type])
    :: Type
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
  Check NullPtr (ValPtr s) = ()
  Check (ValPtr s) NullPtr = ()
  Check (ValPtr s) (ValPtr s) = ()
  Check (ValPtr s) (ValPtr s1) = ()
  Check a a = ()
  Check a b =
    TypeError
      ( Text "Poke the error type, E: "
          :<>: ShowType a
          :<>: Text " A: "
          :<>: ShowType b
      )

type family DeleteList (v :: Type) (ls :: [Type]) :: [Type] where
  DeleteList (ValPtr s) (ValPtr s ': xs) = NullPtr ': DeleteList (ValPtr s) xs
  DeleteList (ValPtr s) (x ': xs) = x ': DeleteList (ValPtr s) xs
  DeleteList (ValPtr s) '[] = '[]

type family DeleteVal (v :: Type) (i :: DM) :: DM where
  DeleteVal v '[] = '[]
  DeleteVal v ((k ':-> ls) ': is) = (k ':-> DeleteList v ls) ': DeleteVal v is

type family CheckJust (ms :: Maybe a) (sym :: Symbol) (errorMsg :: Symbol) :: Constraint where
  CheckJust (Just _) _ _ = ()
  CheckJust Nothing sym errorMsg = TypeError (Text errorMsg :<>: ShowType sym)
data MPtr (ia :: DM -> Type) (b :: DM) where
  MReturn :: ia c -> MPtr ia c
  NewPtr
    :: (Nothing ~ Lookup s dm)
    => Proxy (s :: Symbol)
    -> Struct ts
    -> (At (ValPtr s) (Insert s ts dm) ~> MPtr ia)
    -> MPtr ia dm
  PeekPtr
    :: (Just ts ~ Lookup s dm)
    => ValPtr s
    -> (At (Struct ts) dm ~> MPtr ia)
    -> MPtr ia dm
  PeekPtrField
    :: ( Just ts ~ Lookup s dm
       , val ~ Index n ts
       )
    => ValPtr s
    -> Proxy (n :: Nat)
    -> (At val dm ~> MPtr ia)
    -> MPtr ia dm
  PokePtrField
    :: ( Just ts ~ Lookup s dm
       , val' ~ Index n ts
       , Check val' val
       , newts ~ UpdateIndex n val ts
       , newdm ~ InsertOverwriting s newts dm
       )
    => ValPtr s
    -> Proxy (n :: Nat)
    -> val
    -> MPtr ia newdm
    -> MPtr ia dm
  FreePtr
    :: ( Just ts ~ Lookup s dm
       , newdm ~ DeleteVal (ValPtr s) (Delete s dm)
       )
    => ValPtr s
    -> MPtr ia newdm
    -> MPtr ia dm
  LiftM :: IO (MPtr ia dm) -> MPtr ia dm

instance IFunctor MPtr where
  imap f = \case
    MReturn a -> MReturn (f a)
    NewPtr s st contF -> NewPtr s st (imap f . contF)
    PeekPtr vs contF -> PeekPtr vs (imap f . contF)
    PeekPtrField vs n contF -> PeekPtrField vs n (imap f . contF)
    PokePtrField vs n val cont -> PokePtrField vs n val (imap f cont)
    FreePtr vs cont -> FreePtr vs (imap f cont)
    LiftM ma -> LiftM (fmap (imap f) ma)

instance IMonad MPtr where
  ireturn = MReturn
  ibind f = \case
    MReturn a -> f a
    NewPtr s st contF -> NewPtr s st (ibind f . contF)
    PeekPtr vs contF -> PeekPtr vs (ibind f . contF)
    PeekPtrField vs n contF -> PeekPtrField vs n (ibind f . contF)
    PokePtrField vs n val cont -> PokePtrField vs n val (ibind f cont)
    FreePtr vs cont -> FreePtr vs (ibind f cont)
    LiftM ma -> LiftM (fmap (ibind f) ma)

newptr
  :: forall (s :: Symbol)
    ->(Nothing ~ Lookup s dm)
  => Struct ts -> MPtr (At (ValPtr s) (Insert s ts dm)) dm
newptr s st = NewPtr (Proxy @s) st ireturn

peekptr
  :: (Just ts ~ Lookup s dm)
  => ValPtr s -> MPtr (At (Struct ts) dm) dm
peekptr vs = PeekPtr vs ireturn

peekptrf
  :: ValPtr s
  -> forall (n :: Nat)
    ->(Just ts ~ Lookup s dm)
  => MPtr (At (Index n ts) dm) dm
peekptrf vps n = PeekPtrField vps (Proxy @n) ireturn

pokeptrf
  :: ValPtr s
  -> forall (n :: Nat)
    ->val
  -> ( Just ts ~ Lookup s dm
     , val' ~ Index n ts
     , Check val' val
     , newts ~ UpdateIndex n val ts
     , newdm ~ InsertOverwriting s newts dm
     )
  => MPtr (At () newdm) dm
pokeptrf vps n val = PokePtrField vps (Proxy @n) val (returnAt ())

freeptr
  :: (Just ts ~ Lookup s dm)
  => ValPtr s
  -> MPtr (At () (DeleteVal (ValPtr s) (Delete s dm))) dm
freeptr vs = FreePtr vs (returnAt ())

foo :: MPtr (At () '[]) '[]
foo = I.do
  At k1 <- newptr "k1" (True :& (0 :: Int) :& NullPtrC :& End)
  At k2 <- newptr "k2" (NullPtrC :& (1 :: Double) :& End)
  At k3 <- newptr "k3" (True :& False :& End)
  pokeptrf k1 0 False
  pokeptrf k1 1 (10 :: Int)
  pokeptrf k1 2 k2

  At v12 <- peekptrf k1 2
  pokeptrf v12 1 (10 :: Double)
  pokeptrf v12 0 k1

  pokeptrf k1 2 k3
  At v12' <- peekptrf k1 2
  pokeptrf v12' 0 False
  pokeptrf v12' 1 True
  freeptr k1
  freeptr k2
  freeptr k3
