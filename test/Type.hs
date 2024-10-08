{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Type where

import Data.Type.Map (type (:->) ((:->)))
import Foreign
import TypedPtr

------------Maybe a-------------
instance (Storable a) => Storable (Maybe a) where
  sizeOf x = sizeOf (stripMaybe x) + 1
  alignment x = alignment (stripMaybe x)
  peek ptr = do
    filled <- peekByteOff ptr $ sizeOf $ stripMaybe $ stripPtr ptr
    if filled == (1 :: Word8)
      then do
        x <- peek $ stripMaybePtr ptr
        return $ Just x
      else return Nothing
  poke ptr Nothing = pokeByteOff ptr (sizeOf $ stripMaybe $ stripPtr ptr) (0 :: Word8)
  poke ptr (Just a) = do
    poke (stripMaybePtr ptr) a
    pokeByteOff ptr (sizeOf a) (1 :: Word8)

stripMaybe :: Maybe a -> a
stripMaybe _ = error "stripMaybe"

stripMaybePtr :: Ptr (Maybe a) -> Ptr a
stripMaybePtr = castPtr

stripPtr :: Ptr a -> a
stripPtr _ = error "stripPtr"

--------------------------------

type K1Struct =
  '[ "field0" ':-> Bool
   , "field1" ':-> Int
   , "field2" ':-> NullPtr
   ]

type K2Struct =
  '[ "field0" ':-> NullPtr
   , "field1" ':-> Double
   ]

type K3Struct =
  '[ "field0" ':-> Bool
   , "field1" ':-> Bool
   ]

type U32 = Word32
type U8 = Word8
type CUint = U32

type CUChar = U8
type TcFlag_t = CUint
type CC_t = CUChar
type Speed_t = CUint
type CUShort = Word16
type CULong = Word64
type Ioctl = CULong

type TermSize =
  '[ "row" ':-> CUShort
   , "col" ':-> CUShort
   , "x" ':-> CUShort
   , "y" ':-> CUShort
   ]

type Termios =
  '[ "c_iflag" ':-> TcFlag_t
   , "c_oflag" ':-> TcFlag_t
   , "c_cflag" ':-> TcFlag_t
   , "c_lflag" ':-> TcFlag_t
   , "c_line" ':-> CC_t
   , "c_cc" ':-> Array 32 CC_t
   , "c_ispeed" ':-> Speed_t
   , "c_ospeed" ':-> Speed_t
   ]

defaultTermSize :: Struct (CollVal TermSize)
defaultTermSize =
  80 :& 60 :& 0 :& 0 :& End

defaultTermios :: Struct (CollVal Termios)
defaultTermios =
  0 :& 0 :& 0 :& 0 :& 0 :& (ArrayC [0 .. 31]) :& 0 :& 0 :& End

pattern TIOCGWINSZ :: Ioctl
pattern TIOCGWINSZ = 0x5413
