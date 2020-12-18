{-# language
    NumericUnderscores
  , TemplateHaskell
  , ViewPatterns
#-}

module Conversions
  ( tests
  )
  where

import Control.Monad.Primitive (unsafePrimToPrim)
import Control.Monad.ST (runST)
import Data.ByteString (ByteString)
import Data.ByteString.Short.Internal (ShortByteString(..))
import Data.Bytes.Types
import Data.Maybe (isJust)
import Data.Primitive.ByteArray
import Data.Text (Text)
import Foreign.C.String (CString)
import Foreign.ForeignPtr (withForeignPtr, castForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (castPtr)
import Prelude hiding (String)
import String (String)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.Bytes as Bytes
import qualified Data.Text.Encoding as TE

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified String

import Control.Monad.IO.Class (liftIO)

tests :: IO Bool
tests = checkSequential $$(discover)

prop_roundtripsByteString :: Property
prop_roundtripsByteString
  = roundtrips byteString String.toByteString String.fromByteString

prop_roundtripsByteArray :: Property
prop_roundtripsByteArray
  = roundtrips byteArray String.toByteArray String.fromByteArray

prop_roundtripsShortByteString :: Property
prop_roundtripsShortByteString
  = roundtrips shortByteString String.toShortByteString String.fromShortByteString

prop_roundtripsBytes :: Property
prop_roundtripsBytes
  = roundtrips bytes String.toBytes String.fromBytes

prop_roundtripsCString :: Property
prop_roundtripsCString
  = roundtrips cstring String.toCString String.fromCString

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

bytes :: Gen Bytes
bytes = Bytes.fromByteString <$> Gen.utf8 range genChar
  where
    range = Range.constant 0 1_000
    genChar = Gen.unicode

byteString :: Gen ByteString
byteString = Bytes.toByteString <$> bytes

shortByteString :: Gen ShortByteString
shortByteString = Bytes.toShortByteString <$> bytes

byteArray :: Gen ByteArray
byteArray = Bytes.toByteArray <$> bytes

cstring :: Gen CString
cstring = do
  Bytes buf off len <- bytes
  pure $ runST $ do
    ptr <- unsafePrimToPrim $ mallocBytes len
    copyByteArrayToAddr ptr buf off len
    pure $ castPtr ptr

text :: Gen Text
text = Gen.text range genChar
  where
    range = Range.constant 1 1_000
    genChar = Gen.unicode

{-
TODO: FIX notUtf8 to guarantee it doesn't generate valid UTF-8.
Currently, it can, and hedgehog finds this rather quickly

prop_isUtf8WorksNegative :: Property
prop_isUtf8WorksNegative = property $ do
  nonUtf8Text <- forAll notUtf8
  assert $ not $ isUtf8ByteString nonUtf8Text

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
-}
