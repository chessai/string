{-# language
    BangPatterns
  , DerivingStrategies
  , GeneralizedNewtypeDeriving
  , MagicHash
  , MultiWayIf
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeApplications
  , TypeFamilies
  , UnboxedTuples
  , UnliftedFFITypes
  , ViewPatterns
#-}

module String where

import Control.Monad.Primitive (PrimMonad, PrimState, primitive, unsafePrimToPrim)
import Control.Monad.ST (ST, runST)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString.Short.Internal (ShortByteString(..))
import Data.Bytes.Types (Bytes(..))
import Data.Primitive
import Data.Text.Internal (Text(..))
import Foreign.C.String (CString)
import Foreign.C.Types (CSize(..))
import Foreign.ForeignPtr (withForeignPtr, castForeignPtr)
import Foreign.Ptr (castPtr, minusPtr)
import GHC.Exts hiding (build, toList)
import GHC.Word (Word8(..), Word64(..))
import Numeric (showHex)
import Prelude hiding (String, length, Foldable(..))
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Data.ByteString.Internal as B
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Builder as Builder
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.Text as Text
import qualified Data.Text.Array as TextArray
import qualified Foreign.Marshal.Alloc as Foreign
import qualified Foreign.Marshal.Utils as Foreign
import qualified Foreign.Storable as Foreign
import qualified GHC.Exts as Exts
import qualified Prelude

-----------
-- Types --
-----------

newtype String = String { vec :: Bytes }

---------------
-- Instances --
---------------

deriving newtype instance Eq String
deriving newtype instance Ord String
deriving newtype instance Semigroup String
deriving newtype instance Monoid String

instance IsString String where
  fromString = String.fromList

instance IsList String where
  type Item String = Char
  fromListN = todo -- String.fromListN
  fromList  = String.fromList
  toList    = String.toList

instance Show String where
  show s = "\"" ++ String.toList s ++ "\""

----------------
-- Public API --
----------------

byteLength :: String -> Int
byteLength = coerce Bytes.length

charsLength :: String -> Int
charsLength = String.foldl' (\i _ -> i + 1) 0

-- grapheme length

append :: String -> String -> String
{-# inline append #-}
append (String x) (String y) = String (x <> y)

empty :: String
{-# inline empty #-}
empty = String Bytes.empty

toList :: String -> [Char]
{-# inline toList #-}
toList xs = Exts.build (\c n -> foldr c n xs)

fromList :: [Char] -> String
{-# inline fromList #-}
fromList xs = String
  $ Chunks.concat
  $ Builder.run 4080
  $ Prelude.foldMap Builder.char xs

foldr :: (Char -> b -> b) -> b -> String -> b
{-# inline foldr #-}
foldr f z0 = \s@(String (Bytes _ off0 len0)) ->
  let go !off !len
        | len == 0 = z0
        | otherwise = case nextChar s off of
            Next bytesConsumed c -> f c (go (off + bytesConsumed) (len - bytesConsumed))
  in go off0 len0

foldl' :: (b -> Char -> b) -> b -> String -> b
{-# inline foldl' #-}
foldl' f z0 = \s@(String (Bytes _ off0 len0)) ->
  let go !acc !off !len
        | len == 0 = acc
        | otherwise = case nextChar s off of
            Next bytesConsumed c -> go (f acc c) (off + bytesConsumed) (len - bytesConsumed)
  in go z0 off0 len0

-- (bytesConsumed, charAtPreviousIndex)
data Iter = Next {-# unpack #-} !Int {-# unpack #-} !Char

-- (unsafely) iterate through a 'String'
-- doesn't perform bounds checks. this should be done on
-- a per use-case basis
nextChar :: String -> Int -> Iter
nextChar (String bytes) i =
  let header = Bytes.unsafeIndex bytes i
  in if | iub8  header -> Next 1 (chr8 header)
        | iub16 header -> Next 2 (chr16 header (Bytes.unsafeIndex bytes (i + 1)))
        | iub24 header -> Next 3 (chr24 header (Bytes.unsafeIndex bytes (i + 1)) (Bytes.unsafeIndex bytes (i + 2)))
        | iub32 header -> Next 4 (chr32 header (Bytes.unsafeIndex bytes (i + 1)) (Bytes.unsafeIndex bytes (i + 2)) (Bytes.unsafeIndex bytes (i + 3)))
        | otherwise    -> error $ "Invalid UTF-8 code point: " ++ showHex header ""

chr8 :: Word8 -> Char
chr8 (W8# x) = C# (chr# (word2Int# x))
{-# inline chr8 #-}

chr16 :: Word8 -> Word8 -> Char
chr16 (W8# x1#) (W8# x2#) = C# (chr# (z1# +# z2#))
  where
    !y1# = word2Int# x1#
    !y2# = word2Int# x2#
    !z1# = uncheckedIShiftL# (y1# -# 0xC0#) 6#
    !z2# = y2# -# 0x80#
{-# inline chr16 #-}

chr24 :: Word8 -> Word8 -> Word8 -> Char
chr24 (W8# x1#) (W8# x2#) (W8# x3#) = C# (chr# (z1# +# z2# +# z3#))
  where
    !y1# = word2Int# x1#
    !y2# = word2Int# x2#
    !y3# = word2Int# x3#
    !z1# = uncheckedIShiftL# (y1# -# 0xE0#) 12#
    !z2# = uncheckedIShiftL# (y2# -# 0x80#) 6#
    !z3# = y3# -# 0x80#
{-# inline chr24 #-}

chr32 :: Word8 -> Word8 -> Word8 -> Word8 -> Char
chr32 (W8# x1#) (W8# x2#) (W8# x3#) (W8# x4#) = C# (chr# (z1# +# z2# +# z3# +# z4#))
  where
    !y1# = word2Int# x1#
    !y2# = word2Int# x2#
    !y3# = word2Int# x3#
    !y4# = word2Int# x4#
    !z1# = uncheckedIShiftL# (y1# -# 0xF0#) 18#
    !z2# = uncheckedIShiftL# (y2# -# 0x80#) 12#
    !z3# = uncheckedIShiftL# (y3# -# 0x80#) 6#
    !z4# = y4# -# 0x80#
{-# inline chr32 #-}

todo :: a
todo = error "TODO"

-- Check if the header byte indicates a
-- first-plane byte sequence.
--
-- > all (== True) $ map iub8 [0b0000_0000..0b0111_1111]
-- > all (== False) $ map iub8 [0b1000_0000..maxBound]
--
-- Mask Info
-- Hex:     0x80
-- Binary:  1000 0000
-- Decimal: 128
iub8 :: Word8 -> Bool
iub8 w = w .&. 0x80 == 0
{-# inline iub8 #-}

-- Check if the header byte indicates a
-- second-plane byte sequence.
--
-- > all (== False) $ map iub16 [0b0000_0000..0b1011_1111]
-- > all (== True) $ map iub16 [0b1100_0000..0b1101_1111]
-- > all (== False) $ map iub16 [0b1110_0000..maxBound]
--
-- Mask Info
-- Hex:     0xE0
-- Binary:  1110 0000
-- Decimal: 240
iub16 :: Word8 -> Bool
iub16 w = w .&. 0xE0 == 0xC0
{-# inline iub16 #-}

-- Check if the header byte indicates a
-- third-plane byte sequence.
--
-- > all (== False) $ map iub24 [0b0000_0000..0b1101_1111]
-- > all (== True) $ map iub24 [0b1110_0000..0b1110_1111]
-- > all (== False) $ map iub24 [0b1111_0000..maxBound]
--
-- Mask Info
-- Hex:     0xF0
-- Binary:  1111 0000
-- Decimal: 240
iub24 :: Word8 -> Bool
iub24 w = w .&. 0xF0 == 0xE0
{-# inline iub24 #-}

-- Check if the header byte indicates a
-- fourth-plane byte sequence.
--
-- > all (== False) $ map iub32 [0b0000_0000..0b1110_1111]
-- > all (== True) $ map iub32 [0b1111_0000..0b1111_0111]
-- > all (== False) $ map iub32 [0b1111_1000..maxBound]
--
-- Mask Info
-- Hex:     0xF8
-- Binary:  1111 1000
-- Decimal: 248
iub32 :: Word8 -> Bool
iub32 w = w .&. 0xF8 == 0xF0
{-# inline iub32 #-}

-- Check if the remaining bytes in the sequence
-- adhere to the UTF-8 encoding of b10xxxxxx
--
-- > and $ map iubO [minBound..0b0111_1111] === False
-- > and $ map iubO [0b1100_0000..maxBound] === False
-- > and $ map iubO [0b1000_0000..0b1011_1111] === True

-- Mask Info
-- Hex:     0xC0
-- Binary:  1100 0000
-- Decimal: 192
iubO :: Word8 -> Bool
iubO w = w .&. 0xC0 == 0x80
{-# inline iubO #-}

-----------------
-- Conversions --
-----------------

{-
TODO: Would be great to have multiple encoding support.
Right now all `fromT` expect UTF-8.
-}

-- | Convert a UTF-8-encoded 'ByteString' to a 'String'
fromByteString :: ByteString -> Maybe String
fromByteString b
  | is_utf8 = Just (String (Bytes.fromByteString b))
  | otherwise = Nothing
  where
    is_utf8 = B.accursedUnutterablePerformIO $ do
      let (castForeignPtr -> fp, fromIntegral -> off, fromIntegral -> len) = B.toForeignPtr b
      withForeignPtr fp $ \(Ptr p) -> pure $ isUtf8Ptr p off len
    {-# inline is_utf8 #-}
{-# inline fromByteString #-}

-- | Convert a 'String' to a 'ByteString'
toByteString :: String -> ByteString
toByteString (String b) = Bytes.toByteString b
{-# inline toByteString #-}

-- | Convert a UTF-8-encoded 'ShortByteString' to a 'String'
fromShortByteString :: ShortByteString -> Maybe String
fromShortByteString (SBS b)
  = fromByteArray (ByteArray b)
{-# inline fromShortByteString #-}

-- | Convert a 'String' to a 'ShortByteString'
toShortByteString :: String -> ShortByteString
toShortByteString (String b) = Bytes.toShortByteString b
{-# inline toShortByteString #-}

-- | Convert a UTF-8-encoded 'ByteArray' to a 'String'
fromByteArray :: ByteArray -> Maybe String
fromByteArray b@(ByteArray b#)
  | is_utf8 = Just (String (Bytes.fromByteArray b))
  | otherwise = Nothing
  where
    is_utf8 = isUtf8ByteArray b# 0 (fromIntegral (sizeofByteArray b))
    {-# inline is_utf8 #-}
{-# inline fromByteArray #-}

-- | Convert a 'String' to a 'ByteArray'
toByteArray :: String -> ByteArray
toByteArray (String b) = Bytes.toByteArray b
{-# inline toByteArray #-}

-- | Convert a UTF-8-encoded 'Bytes' to a 'String'
fromBytes :: Bytes -> Maybe String
fromBytes b@(Bytes (ByteArray b#) off len)
  | is_utf8 = Just (String b)
  | otherwise = Nothing
  where
    is_utf8 = isUtf8ByteArray b# (fromIntegral off) (fromIntegral len)
    {-# inline is_utf8 #-}
{-# inline fromBytes #-}

-- | Convert a 'String' to a 'Bytes'
toBytes :: String -> Bytes
toBytes (String b) = b
{-# inline toBytes #-}

-- | Convert a UTF-8-encoded 'CString' to a 'String'
fromCString :: CString -> Maybe String
fromCString (Ptr a)
  | is_utf8 = Just (String (Bytes.fromCString# a))
  | otherwise = Nothing
  where
    is_utf8 = isUtf8Ptr a 0 (fromIntegral (I# (cstringLength# a)))
    {-# inline is_utf8 #-}
{-# inline fromCString #-}

toCString :: String -> CString
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
fromText (Text arr off len)
  | len == 0 = String.empty
  | otherwise = unsafeDupablePerformIO go
  where
    go :: IO String
    go = do
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

copyPtrToByteArray :: PrimMonad m => Ptr a -> MutableByteArray (PrimState m) -> Int -> Int -> m ()
copyPtrToByteArray (Ptr addr) (MutableByteArray arr) (I# off) (I# len)
  = primitive $ \s0 -> case copyAddrToByteArray# addr arr off len s0 of
      s1 -> (# s1, () #)

---------------------
-- Foreign Imports --
---------------------

foreign import ccall unsafe "validation.h run_utf8_validation"
  isUtf8Ptr :: Addr# -> Word64 -> Word64 -> Bool

foreign import ccall unsafe "validation.h run_utf8_validation"
  isUtf8ByteArray :: ByteArray# -> Word64 -> Word64 -> Bool

foreign import ccall unsafe "strlen"
  cstringLength# :: Addr# -> Int#

foreign import ccall unsafe "encode_utf8.h text_encode_utf8"
  c_encode_utf8 :: Ptr (Ptr Word8) -> ByteArray# -> CSize -> CSize -> IO ()
