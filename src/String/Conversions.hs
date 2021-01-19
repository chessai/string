{-# language
    ViewPatterns
#-}

module String.Conversions
  ( fromList, toList
  , fromByteString, toByteString
  , fromShortByteString, toShortByteString
  , fromByteArray, toByteArray
  , fromBytes, toBytes
  , fromCString, toCString
  , fromText, toText
  ) where

import "base" Control.Monad.ST (runST)
import "base" Foreign.C.String (CString)
import "base" Foreign.C.Types (CSize(..))
import "base" Foreign.ForeignPtr (withForeignPtr, castForeignPtr)
import "base" Foreign.Marshal.Alloc qualified as Foreign
import "base" Foreign.Marshal.Utils qualified as Foreign
import "base" Foreign.Ptr (castPtr, minusPtr)
import "base" Foreign.Storable qualified as Foreign
import "base" GHC.Exts hiding (build, toList, fromList)
import "base" GHC.Exts qualified as Exts
import "base" GHC.Word (Word8(..), Word64(..))
import "base" Prelude hiding (String, length, Foldable(..))
import "base" System.IO.Unsafe (unsafeDupablePerformIO)
import "bytebuild" Data.Bytes.Builder qualified as Builder
import "byteslice" Data.Bytes qualified as Bytes
import "byteslice" Data.Bytes.Chunks qualified as Chunks
import "byteslice" Data.Bytes.Types (Bytes(..))
import "bytestring" Data.ByteString (ByteString)
import "bytestring" Data.ByteString.Internal qualified as B
import "bytestring" Data.ByteString.Short.Internal (ShortByteString(..))
import "primitive" Control.Monad.Primitive (PrimMonad, PrimState, primitive, unsafePrimToPrim)
import "primitive" Data.Primitive.ByteArray (ByteArray(..), MutableByteArray(..), newByteArray, unsafeFreezeByteArray, copyByteArrayToAddr, sizeofByteArray)
import "text" Data.Text.Array qualified as TextArray
import "text" Data.Text.Internal (Text(..))
import "this" String.Folds qualified as Folds
import "this" String.Type
import Prelude hiding (String)

toList :: String -> [Char]
{-# inline toList #-}
toList xs = Exts.build (\c n -> Folds.foldr c n xs)

fromList :: [Char] -> String
{-# inline fromList #-}
fromList xs = String
  $ Chunks.concat
  $ Builder.run 4080
  $ Prelude.foldMap Builder.char xs

-- | Convert a UTF-8-encoded 'ByteString' to a 'String'
fromByteString :: ByteString -> Maybe String
{-# inline fromByteString #-}
fromByteString b
  | is_utf8 = Just (String (Bytes.fromByteString b))
  | otherwise = Nothing
  where
    is_utf8 = B.accursedUnutterablePerformIO $ do
      let (castForeignPtr -> fp, fromIntegral -> off, fromIntegral -> len) = B.toForeignPtr b
      withForeignPtr fp $ \(Ptr p) -> pure $ isUtf8Ptr p off len
    {-# inline is_utf8 #-}

-- | Convert a 'String' to a 'ByteString'
toByteString :: String -> ByteString
{-# inline toByteString #-}
toByteString (String b) = Bytes.toByteString b

-- | Convert a UTF-8-encoded 'ShortByteString' to a 'String'
fromShortByteString :: ShortByteString -> Maybe String
{-# inline fromShortByteString #-}
fromShortByteString (SBS b)
  = fromByteArray (ByteArray b)

-- | Convert a 'String' to a 'ShortByteString'
toShortByteString :: String -> ShortByteString
{-# inline toShortByteString #-}
toShortByteString (String b) = Bytes.toShortByteString b

-- | Convert a UTF-8-encoded 'ByteArray' to a 'String'
fromByteArray :: ByteArray -> Maybe String
{-# inline fromByteArray #-}
fromByteArray b@(ByteArray b#)
  | is_utf8 = Just (String (Bytes.fromByteArray b))
  | otherwise = Nothing
  where
    is_utf8 = isUtf8ByteArray b# 0 (fromIntegral (sizeofByteArray b))
    {-# inline is_utf8 #-}

-- | Convert a 'String' to a 'ByteArray'
toByteArray :: String -> ByteArray
{-# inline toByteArray #-}
toByteArray (String b) = Bytes.toByteArray b

-- | Convert a UTF-8-encoded 'Bytes' to a 'String'
fromBytes :: Bytes -> Maybe String
{-# inline fromBytes #-}
fromBytes b@(Bytes (ByteArray b#) off len)
  | is_utf8 = Just (String b)
  | otherwise = Nothing
  where
    is_utf8 = isUtf8ByteArray b# (fromIntegral off) (fromIntegral len)
    {-# inline is_utf8 #-}

-- | Convert a 'String' to a 'Bytes'
toBytes :: String -> Bytes
{-# inline toBytes #-}
toBytes (String b) = b

-- | Convert a UTF-8-encoded 'CString' to a 'String'
fromCString :: CString -> Maybe String
{-# inline fromCString #-}
fromCString (Ptr a)
  | is_utf8 = Just (String (Bytes.fromCString# a))
  | otherwise = Nothing
  where
    is_utf8 = isUtf8Ptr a 0 (fromIntegral (I# (cstringLength# a)))
    {-# inline is_utf8 #-}

toCString :: String -> CString
{-# inline toCString #-}
toCString (String (Bytes buf off len)) = runST $ do
  ptr <- unsafePrimToPrim $ Foreign.mallocBytes len
  copyByteArrayToAddr ptr buf off len
  pure $ castPtr ptr

-- | Convert a UTF-16-encoded 'Text' to a 'String'
--
--   /Note/: This does not return a 'Maybe' because
--           'Text' is always well-formed UTF-16 unless
--           constructed via some unsafe methodology.
fromText :: Text -> String
{-# inline fromText #-}
fromText (Text arr off len)
  | len == 0 = String (Bytes.empty)
  | otherwise = unsafeDupablePerformIO $ do
      -- number of bytes to allocate per code unit.
      -- maximum expansion from UTF-16 -> UTF-8 is
      -- 50%, as characters < 0xFFFF, which are all
      -- encoded with as one code unit, get encoded
      -- with at most 3 bytes in UTF-8.
      let allocPerUnit = 3
      ptr <- Foreign.mallocBytes (len * allocPerUnit)
      Foreign.with ptr $ \destPtr -> do
        c_encode_utf8 destPtr (TextArray.aBA arr) (fromIntegral off) (fromIntegral len)
        newDest <- Foreign.peek destPtr
        let utf8Len = newDest `minusPtr` ptr
        mbuf <- newByteArray utf8Len
        copyPtrToByteArray ptr mbuf 0 utf8Len
        buf <- unsafeFreezeByteArray mbuf
        pure (String (Bytes buf 0 utf8Len))

-- | Convert a 'String' to a UTF-16 encoded 'Text'
toText :: String -> Text
{-# inline toText #-}
toText _ = error "TODO"

----------------------
-- Internal Helpers --
----------------------

copyPtrToByteArray :: PrimMonad m => Ptr a -> MutableByteArray (PrimState m) -> Int -> Int -> m ()
{-# inline copyPtrToByteArray #-}
copyPtrToByteArray (Ptr addr) (MutableByteArray arr) (I# off) (I# len)
  = primitive $ \s0 -> case copyAddrToByteArray# addr arr off len s0 of
      s1 -> (# s1, () #)

foreign import ccall unsafe "validation.h run_utf8_validation"
  isUtf8Ptr :: Addr# -> Word64 -> Word64 -> Bool

foreign import ccall unsafe "validation.h run_utf8_validation"
  isUtf8ByteArray :: ByteArray# -> Word64 -> Word64 -> Bool

foreign import ccall unsafe "strlen"
  cstringLength# :: Addr# -> Int#

foreign import ccall unsafe "encode_utf8.h text_encode_utf8"
  c_encode_utf8 :: Ptr (Ptr Word8) -> ByteArray# -> CSize -> CSize -> IO ()
