{-# language
    NumericUnderscores
  , TemplateHaskell
  , ViewPatterns
#-}

module Conversions
  ( tests
  )
  where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.Text.Encoding as TE
import Foreign.ForeignPtr (withForeignPtr, castForeignPtr)
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (isJust)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified String

tests :: IO Bool
tests = checkSequential $$(discover)

isUtf8ByteString :: ByteString -> Bool
isUtf8ByteString b = unsafePerformIO $ do
  let (castForeignPtr -> fp, fromIntegral -> off, fromIntegral -> len) = B.toForeignPtr b
  withForeignPtr fp $ \p -> pure $ String.isUtf8Ptr p off len
{-# noinline isUtf8ByteString #-} -- should be safe regardless but w/e

prop_isUtf8WorksPositive :: Property
prop_isUtf8WorksPositive = property $ do
  utf8Text <- forAll utf8
  assert $ isUtf8ByteString utf8Text
  assert $ isJust $ String.fromByteString utf8Text

{-
TODO: FIX notUtf8 to guarantee it doesn't generate valid UTF-8.
Currently, it can, and hedgehog finds this rather quickly
prop_isUtf8WorksNegative :: Property
prop_isUtf8WorksNegative = property $ do
  nonUtf8Text <- forAll notUtf8
  assert $ not $ isUtf8ByteString nonUtf8Text
-}

utf8 :: Gen ByteString
utf8 = Gen.utf8 range genChar
  where
    range = Range.constant 0 10_000
    genChar = Gen.unicode

notUtf8 :: Gen ByteString
notUtf8 = do
  nonUtf8Encoding <- Gen.element
    [ TE.encodeUtf16LE
    , TE.encodeUtf16BE
    , TE.encodeUtf32LE
    , TE.encodeUtf32BE
    ]
  nonUtf8Encoding <$> Gen.text range genChar
  where
    range = Range.constant 1 10_000 -- empty string is UTF-8
    genChar = Gen.unicode
