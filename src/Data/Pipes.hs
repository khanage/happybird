{-# LANGUAGE TemplateHaskell #-}
module Data.Pipes where

import Control.Lens
import           Prelude
import           System.Random (StdGen)

data Pipes = Pipes { _pGen   :: StdGen
                   , _pPipes :: [(Float, Float)]
                   } deriving (Show)

makeLenses ''Pipes

-- | Not at all accurate, but some handwaiving
-- | is neccesary due to World requiring Eq
instance Eq Pipes where
  (Pipes _ leftPs) == (Pipes _ rightPs) =
    leftPs == rightPs
