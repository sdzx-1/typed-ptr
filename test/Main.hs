{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QualifiedDo #-}

module Main (main) where

import Control.Monad (replicateM_)
import Data.IFunctor (At (..))
import qualified Data.IFunctor as I
import TypedPtr

main :: IO ()
main = do
  putStrLn "Test suite not yet implemented."
  replicateM_ 100 $ runMPtr foo

foo :: MPtr (At () '[]) '[]
foo = I.do
  At k1 <- newptr "k1" (True :& (0 :: Int) :& NullPtrC :& End)
  At k2 <- newptr "k2" (NullPtrC :& (1 :: Double) :& End)
  At k3 <- newptr "k3" (True :& False :& End)

  liftm $ print (k1, k2, k3)

  -- At v1 <- peekptr k1
  -- At v2 <- peekptr k2
  -- At v3 <- peekptr k3
  -- liftm $ print (v1, v2, v3)

  pokeptrf k1 0 False
  pokeptrf k1 1 (10 :: Int)
  pokeptrf k1 2 k2

  -- At v1 <- peekptr k1
  -- At v2 <- peekptr k2
  -- At v3 <- peekptr k3
  -- liftm $ print (v1, v2, v3)

  At v12 <- peekptrf k1 2
  pokeptrf v12 1 (10085 :: Double)
  pokeptrf v12 0 k1

  -- At v1 <- peekptr k1
  -- At v2 <- peekptr k2
  -- At v3 <- peekptr k3
  -- liftm $ print (v1, v2, v3)

  pokeptrf k1 2 k3
  At v12' <- peekptrf k1 2
  pokeptrf v12' 0 False
  pokeptrf v12' 1 True

  At v1 <- peekptr k1
  At v2 <- peekptr k2
  At v3 <- peekptr k3
  liftm $ print (v1, v2, v3)

  freeptr k1
  freeptr k2
  freeptr k3
