module Main (main) where

import "base" Data.Coerce (coerce)
import "base" Data.Monoid (All(..))
import Prelude hiding (String)
import "string" String (String)

import "this" Conversions qualified
import "this" Typeclasses qualified

main :: IO Bool
main = do
  testAll
    [ Conversions.tests
    , Typeclasses.tests
    ]

testAll :: [IO Bool] -> IO Bool
testAll = id
  . coerce @_ @(IO Bool)
  . foldMap id
  . coerce @_ @[IO All]
