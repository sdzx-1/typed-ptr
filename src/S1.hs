{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeApplications #-}

module S1 where

import Foreign
import Foreign.CStorable
import GHC.Generics (Generic)

storInfo :: forall a -> (Storable a) => (Int, Int)
storInfo a = (sizeOf @a undefined, alignment @a undefined)

data T = TC
  { val1 :: Int
  , val2 :: Double
  , val21 :: Word8
  , val22 :: Word8
  , val23 :: Word8
  , val3 :: Float
  }
  deriving (Show, Generic, CStorable)

instance Storable T where
  peek = cPeek
  poke = cPoke
  alignment = cAlignment
  sizeOf = cSizeOf
