{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
import Data.Proxy
import Data.Type.Map (Insert, Lookup, (:->) (..))
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
    -> Ptr (Struct TermSize)
    -> IO CInt

foreign import ccall unsafe "isatty"
  c_isatty :: CInt -> IO CInt

foreign import ccall unsafe "ttyname"
  c_ttyname :: CInt -> IO CString

foreign import ccall unsafe "tcgetattr"
  c_tcgetattr
    :: CInt
    -> (Ptr (Struct Termios))
    -> IO Int

pattern TCSANOW :: CInt
pattern TCSANOW = 0

foreign import ccall unsafe "cfmakeraw"
  c_cfmakeraw :: (Ptr (Struct Termios)) -> IO ()

foreign import ccall unsafe "tcsetattr"
  c_tcsetattr
    :: CInt
    -> CInt
    -> Ptr (Struct Termios)
    -> IO Int

tcsetattr
  :: CInt
  -> Ptr (Struct Termios)
  -> IO Int
tcsetattr fd ptr =
  c_tcsetattr fd TCSANOW ptr

foo :: MPtr (At () '[]) '[]
foo = I.do
  let FD{fdFD} = stdout
  At rawTerminal <-
    newStructPtr
      "rawTerminal"
      RawTerminal
      ((Proxy, defaultTermios) :& (Proxy, fdFD) :& End)
  printptr "init RawTerminal" rawTerminal

  At fd <- peekptr rawTerminal.output
  -- temp termios
  At termiosPtr <- newStructPtr "termios" Termios defaultTermios
  printptr "init Termios" termiosPtr
  -- get current termios
  ptrffi termiosPtr (c_tcgetattr fd)
  printptr "tcgetatt" termiosPtr
  -- copy termios to rawTerminal.prev_ios
  copyStruct rawTerminal.prev_ios termiosPtr
  printptr "copy" rawTerminal
  -- set termios to raw
  ptrffi termiosPtr c_cfmakeraw
  -- set stdout to raw
  ptrffi termiosPtr (tcsetattr fd)
  -- free temp termios
  freeptr termiosPtr
  At termSize <- newStructPtr "termSize" TermSize defaultTermSize
  ptrffi termSize (c_ioctl fd TIOCGWINSZ)
  printptr "TermSize" termSize
  freeptr termSize
  -- restore stdout
  ptrffi (rawTerminal.prev_ios) (tcsetattr fd)
  freeptr rawTerminal
