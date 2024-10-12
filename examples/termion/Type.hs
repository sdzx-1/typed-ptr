{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Type where

import Data.Proxy
import Data.Type.Map (type (:->) ((:->)))
import Foreign
import Foreign.C (CInt)
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
type USize = Word64

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

type instance Alignment CInt = 4
type instance Size CInt = 4

type RawTerminal =
  '[ "prev_ios" ':-> Struct Termios
   , "output" ':-> CInt -- stdout fd
   ]

defaultRawTerminal :: Struct RawTerminal
defaultRawTerminal =
  (Proxy, defaultTermios)
    :& (Proxy, 0)
    :& End

defaultTermSize :: Struct TermSize
defaultTermSize =
  (Proxy, 80)
    :& (Proxy, 60)
    :& (Proxy, 0)
    :& (Proxy, 0)
    :& End

defaultTermios :: Struct Termios
defaultTermios =
  (Proxy, 0)
    :& (Proxy, 0)
    :& (Proxy, 0)
    :& (Proxy, 0)
    :& (Proxy, 0)
    :& (Proxy, ArrayC [0 ..])
    :& (Proxy, 0)
    :& (Proxy, 0)
    :& End

pattern TIOCGWINSZ :: Ioctl
pattern TIOCGWINSZ = 0x5413

data Buffer = Buffer Word64

instance Storable Buffer where
  sizeOf (Buffer i) = fromIntegral i
  alignment _ = 1
  peek _ = error "Never peek Buffer"
  poke _ _ = error "Never poke Buffer"

type Str s =
  '[ "ptr" ':-> ValPtr s
   , "len" ':-> USize
   ]
