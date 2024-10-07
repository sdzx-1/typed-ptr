{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Monad (replicateM_)
import Data.IFunctor (At (..))
import qualified Data.IFunctor as I
import Data.Type.Map (type (:->) ((:->)))
import TypedPtr

main :: IO ()
main = do
  putStrLn "Test suite not yet implemented."
  replicateM_ 100 $ runMPtr foo

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
