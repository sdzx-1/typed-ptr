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

type U32 = Word32
type U8 = Word8
type CUint = U32

type CUChar = U8
type TcFlag_t = CUint
type CC_t = U8
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
  0 :& 0 :& 0 :& 0 :& 0 :& (ArrayC [0 ..]) :& 0 :& 0 :& End

pattern TIOCGWINSZ :: Ioctl
pattern TIOCGWINSZ = 0x5413
