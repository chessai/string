{-# language
    TemplateHaskell
  , ViewPatterns
#-}

{-# options_ghc -Wall #-}

module Conversions
  ( tests
  )
  where

import "hedgehog" Hedgehog
import "string" String (String)
import "this" Generators qualified
import Prelude hiding (String)
import String qualified

tests :: IO Bool
tests = checkSequential $$(discover)

prop_roundtripsByteString :: Property
prop_roundtripsByteString
  = roundtrips
      Generators.byteString
      String.toByteString
      String.fromByteString

prop_roundtripsByteArray :: Property
prop_roundtripsByteArray
  = roundtrips
      Generators.byteArray
      String.toByteArray
      String.fromByteArray

prop_roundtripsShortByteString :: Property
prop_roundtripsShortByteString
  = roundtrips
      Generators.shortByteString
      String.toShortByteString
      String.fromShortByteString

prop_roundtripsBytes :: Property
prop_roundtripsBytes
  = roundtrips
      Generators.bytes
      String.toBytes
      String.fromBytes

{-
prop_roundtripsCString :: Property
prop_roundtripsCString
  = roundtrips
      Generators.cstring
      String.toCString
      String.fromCString
-}

roundtrips :: (Show bytes)
  => Gen bytes
  -> (String -> bytes) -- to
  -> (bytes -> Maybe String) -- from
  -> Property
roundtrips genBytes to from = property $ do
  bytes <- forAll genBytes
  string <- case from bytes of
    Nothing -> do
      footnote "decoder failed"
      failure
    Just string -> do
      pure string
  tripping string to from
