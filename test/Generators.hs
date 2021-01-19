{-# language
    NumericUnderscores
  , TemplateHaskell
  , ViewPatterns
#-}

-- TODO: generators with offset/length
-- TODO: 'string' generator should go through multiple
--       possible types
module Generators
  ( string
  , bytes
  , byteString
  , shortByteString
  , byteArray
  , cstring
  , text
  , listChar
  ) where

import "base" Control.Monad.IO.Class (liftIO)
import "base" Control.Monad.ST (runST)
import "base" Data.Maybe (isJust)
import "base" Foreign.C.String (CString)
import "base" Foreign.ForeignPtr (withForeignPtr, castForeignPtr)
import "base" Foreign.Marshal.Alloc (mallocBytes)
import "base" Foreign.Ptr (castPtr)
import "base" System.IO.Unsafe (unsafePerformIO)
import "byteslice" Data.Bytes qualified as Bytes
import "byteslice" Data.Bytes.Types
import "bytestring" Data.ByteString (ByteString)
import "bytestring" Data.ByteString qualified as B
import "bytestring" Data.ByteString.Internal qualified as B
import "bytestring" Data.ByteString.Short.Internal (ShortByteString(..))
import "hedgehog" Hedgehog
import "hedgehog" Hedgehog.Gen qualified as Gen
import "hedgehog" Hedgehog.Range qualified as Range
import "primitive" Control.Monad.Primitive (unsafePrimToPrim)
import "primitive" Data.Primitive.ByteArray
import "string" String
import "text" Data.Text (Text)
import "text" Data.Text.Encoding qualified as TE
import Prelude hiding (String)

string :: Gen String
string = Gen.just $ fmap String.fromBytes $ bytes

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

listChar :: Gen [Char]
listChar = Gen.string range genChar
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
