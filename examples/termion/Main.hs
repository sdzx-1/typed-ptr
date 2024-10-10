{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Data.IFunctor (At (..))
import qualified Data.IFunctor as I
import Data.Type.Map (Insert, (:->) (..))
import Foreign
import Foreign.C hiding (CUChar, CULong, CUShort)
import GHC.IO.Device (IODeviceType (..))
import GHC.IO.FD (FD (..), openFile, stdout)
import GHC.IO.IOMode (IOMode (..))
import GHC.TypeLits (AppendSymbol, KnownNat)
import Type
import TypedPtr

main :: IO ()
main = runMPtr foo

foreign import ccall unsafe "ioctl"
  c_ioctl
    :: CInt
    -> Ioctl
    -> Ptr (Struct (CollVal TermSize))
    -> IO CInt

foreign import ccall unsafe "isatty"
  c_isatty :: CInt -> IO CInt

foreign import ccall unsafe "ttyname"
  c_ttyname :: CInt -> IO CString

foreign import ccall unsafe "tcgetattr"
  c_tcgetattr
    :: CInt
    -> (Ptr (Struct (CollVal Termios)))
    -> IO Int

pattern TCSANOW :: CInt
pattern TCSANOW = 0

foreign import ccall unsafe "cfmakeraw"
  c_cfmakeraw :: (Ptr (Struct (CollVal Termios))) -> IO ()

foreign import ccall unsafe "tcsetattr"
  c_tcsetattr
    :: CInt
    -> CInt
    -> Ptr (Struct (CollVal Termios))
    -> IO Int

tcsetattr
  :: CInt
  -> Ptr (Struct (CollVal Termios))
  -> IO Int
tcsetattr fd ptr =
  c_tcsetattr fd TCSANOW ptr

copyStruct
  :: forall ts
   . ( KnownNat (ListMaxAlignment 0 ts)
     , KnownNat (Last (Acc0 0 ts ts))
     , ReifyOffsets (Init (Acc0 0 ts ts))
     , PeekStruct ts
     )
  => Ptr (Struct ts) -> Ptr (Struct ts) -> IO ()
copyStruct dest source =
  copyBytes dest source (sizeOf @(Struct ts) undefined)

foo :: MPtr (At () '[]) '[]
foo = I.do
  let FD{fdFD} = stdout
  At rawTerminal <- newStructPtr "rawTerminal" RawTerminal (defaultTermios :& fdFD :& End)
  At fd <- peekStructf rawTerminal "output"

  -- temp termios
  At termiosPtr <- newStructPtr "termios" Termios defaultTermios
  At termiosStructPtr <- toPtrStruct termiosPtr

  -- get current termios
  liftm $ c_tcgetattr fd termiosStructPtr

  At newptr <- toPtrStructField rawTerminal "prev_ios"
  -- copy termios to rawTerminal prev_ios
  liftm $ copyStruct newptr termiosStructPtr

  -- set termios ptr to raw
  liftm $ c_cfmakeraw termiosStructPtr
  -- set stdout to raw
  liftm $ tcsetattr fd termiosStructPtr
  -- free temp termios
  freeptr termiosPtr

  -- stdout restore
  liftm $ tcsetattr fd newptr

  freeptr rawTerminal
  liftm $ threadDelay 3000000

-- let bufferSize = 100
-- At bufferPtr <- newSingletonPtr "Buff" (Buffer bufferSize)
-- At strPtr <- newStructPtr "str" (Str _) (bufferPtr :& bufferSize :& End)
-- At val2 <- peekStruct strPtr
-- liftm $ print val2
-- pokeStructf strPtr "ptr" bufferPtr
-- pokeStructf strPtr "len" (32 :: Word64)
-- freeptr strPtr
-- freeptr bufferPtr

subPtr
  :: (s1 ~ AppendSymbol s "-slice")
  => ValPtr s
  -> MPtr (At (ValPtr s1) dm) dm
subPtr = undefined
