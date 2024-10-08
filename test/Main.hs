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

module Main (main) where

import Control.Concurrent (threadDelay)
import Data.IFunctor (At (..))
import qualified Data.IFunctor as I
import Foreign
import Foreign.C hiding (CUChar, CULong, CUShort)
import GHC.IO.Device (IODeviceType (..))
import GHC.IO.FD (FD (..), openFile, release)
import GHC.IO.IOMode (IOMode (..))
import Type
import TypedPtr

main :: IO ()
main = do
  -- replicateM_ 100 $ runMPtr foo
  foo1

foo :: MPtr (At () '[]) '[]
foo = I.do
  At k1 <- newptr "k1" K1Struct (True :& 0 :& NullPtrC :& End)
  At k2 <- newptr "k2" K2Struct (NullPtrC :& 1 :& End)
  At k3 <- newptr "k3" K3Struct (True :& False :& End)

  liftm $ print (k1, k2, k3)

  -- At v1 <- peekptr k1
  -- At v2 <- peekptr k2
  -- At v3 <- peekptr k3
  -- liftm $ print (v1, v2, v3)

  pokeptrf k1 "field0" False
  pokeptrf k1 "field1" (10 :: Int)
  pokeptrf k1 "field2" k2

  -- At v1 <- peekptr k1
  -- At v2 <- peekptr k2
  -- At v3 <- peekptr k3
  -- liftm $ print (v1, v2, v3)

  At v12 <- peekptrf k1 "field2"
  pokeptrf v12 "field1" (10085 :: Double)
  pokeptrf v12 "field0" k1

  -- At v1 <- peekptr k1
  -- At v2 <- peekptr k2
  -- At v3 <- peekptr k3
  -- liftm $ print (v1, v2, v3)

  pokeptrf k1 "field2" k3
  At v12' <- peekptrf k1 "field2"
  pokeptrf v12' "field0" False
  pokeptrf v12' "field1" True

  At v1 <- peekptr k1
  At v2 <- peekptr k2
  At v3 <- peekptr k3
  liftm $ print (v1, v2, v3)

  freeptr k1
  freeptr k2
  freeptr k3

foreign import ccall unsafe "ioctl"
  c_ioctl
    :: CInt
    -> Ioctl
    -> Ptr (Struct (CollVal TermSize))
    -> IO CInt

showIOT :: IODeviceType -> String
showIOT = \case
  Directory -> "Directory"
  Stream -> "Stream"
  RegularFile -> "RegularFile"
  RawDevice -> "RawDevice"

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

tcgetattr :: FD -> Ptr (Struct (CollVal Termios)) -> IO Bool
tcgetattr FD{fdFD} ptr = do
  val <- c_tcgetattr fdFD ptr
  pure $ case val of
    0 -> True
    _ -> False

ttyname :: FD -> IO String
ttyname FD{fdFD} = do
  ptr <- c_ttyname fdFD
  peekCString ptr

isatty :: FD -> IO Bool
isatty FD{fdFD} = do
  val <- c_isatty fdFD
  pure $ case val of
    0 -> False
    _ -> True

foo1 :: IO ()
foo1 = do
  (fd@FD{fdFD}, iot) <-
    openFile
      "/dev/tty"
      ReadWriteMode
      False
  print fd
  putStrLn $ showIOT iot
  checkVal <- isatty fd
  print checkVal
  name <- ttyname fd
  print name
  ptr <- malloc @(Struct (CollVal Termios))
  ptr1 <- malloc @(Struct (CollVal TermSize))
  peek ptr >>= print
  poke ptr defaultTermios
  peek ptr >>= print
  val <- tcgetattr fd ptr
  print val
  peek ptr >>= print
  v1 <- c_ioctl fdFD TIOCGWINSZ ptr1
  print ("TermSizeResult", v1)
  peek ptr1 >>= print

  threadDelay 3_000_000
  free ptr
  free ptr1
  release fd
  print "finish"
