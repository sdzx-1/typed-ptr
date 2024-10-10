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
{-# OPTIONS_GHC -Wname-shadowing #-}

module Main (main) where

import Data.IFunctor (At (..))
import qualified Data.IFunctor as I
import Foreign
import Foreign.C hiding (CUChar, CULong, CUShort)
import GHC.IO.Device (IODeviceType (..))
import GHC.IO.FD (FD (..), openFile)
import GHC.IO.IOMode (IOMode (..))
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

foreign import ccall unsafe "tcsetattr"
  c_tcsetattr
    :: CInt
    -> CInt
    -> (Ptr (Struct (CollVal Termios)))
    -> IO Int

tcsetattr
  :: CInt
  -> Ptr (Struct (CollVal Termios))
  -> IO Int
tcsetattr fd ptr =
  c_tcsetattr fd TCSANOW ptr

foo :: MPtr (At () '[]) '[]
foo = I.do
  At termiosPtr <- newStructPtr "termios" Termios defaultTermios
  At termSizePtr <- newStructPtr "termSize" TermSize defaultTermSize

  At (FD{fdFD}, _) <-
    liftm $ openFile "/dev/tty" ReadWriteMode False

  At ptrStruct <- toPtrStruct termiosPtr
  liftm $ c_tcgetattr fdFD ptrStruct

  At ptrStructTermSize <- toPtrStruct termSizePtr
  liftm $ c_ioctl fdFD TIOCGWINSZ ptrStructTermSize

  At val <- peekStruct termiosPtr
  liftm $ print ("termios", val)

  At val1 <- peekStruct termSizePtr
  liftm $ print ("termSize", val1)

  let bufferSize = 100
  At bufferPtr <- newSingletonPtr "Buff" (Buffer bufferSize)
  At strPtr <- newStructPtr "str" (Str _) (bufferPtr :& bufferSize :& End)

  At val2 <- peekStruct strPtr
  liftm $ print val2

  freeptr termSizePtr
  freeptr termiosPtr
  freeptr strPtr
  freeptr bufferPtr
