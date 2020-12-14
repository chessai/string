{-# language
    OverloadedStrings
  , TypeApplications
#-}

{-# options_ghc -O2 #-}

module Main (main) where

import Data.Coerce (coerce)
import Data.Monoid (All(..))
import Prelude hiding (String)
import String (String)
import qualified String

import qualified Conversions

main :: IO Bool
main = do
  testAll
    [ Conversions.tests
    ]

testAll :: [IO Bool] -> IO Bool
testAll = id
  . coerce @_ @(IO Bool)
  . foldMap id
  . coerce @_ @[IO All]
