{-# language
    TypeFamilies
  , ViewPatterns
#-}

module String
  ( String

    -- * Conversions
  , fromList, toList
  , fromByteString, toByteString
  , fromShortByteString, toShortByteString
  , fromByteArray, toByteArray
  , fromBytes, toBytes
  , fromCString, toCString
  , fromText, toText

    -- * Folds
  , foldr
  , foldl'
  , foldMap
  , foldMap'

  ) where

import "base" Control.Applicative (pure, (*>))
import "base" Control.Monad.ST (runST)
import "base" Data.Bits ((.&.))
import "base" Data.Bool (Bool(..), otherwise)
import "base" Data.Coerce (coerce)
import "base" Data.Eq (Eq(..))
import "base" Control.Monad (Monad, (=<<))
import "base" Data.Function (($), (.), id, flip)
import "base" Data.Functor (fmap)
import "base" Data.List ((++))
import "base" Data.Maybe (Maybe(..))
import "base" Data.Monoid (Monoid, mempty)
import "base" Data.Monoid (Sum(..))
import "base" Data.Ord (Ord)
import "base" Data.Semigroup (Semigroup, (<>))
import "base" Data.String (IsString(..))
import "base" Foreign.C.String (CString)
import "base" Control.Monad.Fail (fail)
import "base" Foreign.C.Types (CSize(..))
import "base" Foreign.ForeignPtr (withForeignPtr, castForeignPtr)
import "base" Foreign.Marshal.Alloc qualified as Foreign
import "base" Foreign.Marshal.Utils qualified as Foreign
import "base" Foreign.Ptr (castPtr, minusPtr)
import "base" Foreign.Storable qualified as Foreign
import "base" GHC.Err (error)
import "base" GHC.Exts (IsList, ByteArray#, Ptr(..), Int#, Addr#, RealWorld, Char(..), copyAddrToByteArray#, (+#), (-#), uncheckedIShiftL#, word2Int#, chr#)
import "base" GHC.Exts qualified as Exts
import "base" GHC.Generics (Generic)
import "base" GHC.IO (IO(..))
import "base" GHC.Int (Int(..))
import "base" GHC.Num ((+), (*), (-))
import "base" GHC.Read (Read(..))
import "base" GHC.Read qualified as R
import "base" GHC.Real (fromIntegral)
import "base" GHC.Word (Word8(..), Word64(..))
import "base" Numeric (showHex)
import "base" System.IO.Unsafe (unsafeDupablePerformIO)
import "base" Text.ParserCombinators.ReadPrec (ReadPrec)
import "base" Text.ParserCombinators.ReadPrec qualified as R
import "base" Text.Read qualified as R
import "base" Text.Show
import "binary" Data.Binary (Binary(..))
import "binary" Data.Binary qualified as Binary
import "bytebuild" Data.Bytes.Builder qualified as Builder
import "byteslice" Data.Bytes (Bytes)
import "byteslice" Data.Bytes qualified as Bytes
import "byteslice" Data.Bytes.Chunks qualified as Chunks
import "byteslice" Data.Bytes.Types (Bytes(..))
import "bytestring" Data.ByteString (ByteString)
import "bytestring" Data.ByteString.Internal qualified as B
import "bytestring" Data.ByteString.Short.Internal (ShortByteString(..))
import "deepseq" Control.DeepSeq (NFData(..))
import "primitive" Control.Monad.Primitive (unsafePrimToPrim)
import "primitive" Data.Primitive.ByteArray (ByteArray(..), MutableByteArray(..), newByteArray, unsafeFreezeByteArray, copyByteArrayToAddr, sizeofByteArray)
import "text" Data.Text.Array qualified as TextArray
import "text" Data.Text.Internal (Text(..))
import Prelude qualified

---------------------
-- Types/Instances --
---------------------

newtype String = String Bytes

deriving newtype instance Eq String
deriving newtype instance Ord String
deriving newtype instance Semigroup String
deriving newtype instance Monoid String
deriving stock instance Generic String

instance Show String where
  showsPrec :: Int -> String -> ShowS
  showsPrec _ s =  id
    . showChar '\"'
    . showString (toList s)
    . showChar '\"'

instance Read String where
  readPrec :: ReadPrec String
  readPrec = do
    fmap fromList $ R.parens $ do
      R.expectP (R.Char '\"')
      rest False R.+++ next
    where
      rest started = do
        R.Char c <- R.lexP
        case c of
          '\"' -> pure []
          _ -> if started then next else R.pfail
      next = do
        x <- R.reset (readPrec @Char)
        xs <- rest True
        pure (x:xs)


instance IsString String where
  fromString :: [Char] -> String
  {-# inline fromString #-}
  fromString xs = String
    $ Chunks.concat
    $ Builder.run 4080
    $ Prelude.foldMap Builder.char xs

instance IsList String where
  type Item String = Char
  toList = toList
  fromList = fromList
  -- TODO: efficient fromListN

instance NFData String where
  rnf !_ = ()

instance Binary String where
  put s = do
    put (byteLength s)
    Bytes.foldl'
      (\p b -> p *> Binary.putWord8 b)
      (pure ())
      (toBytes s)
  get = do
    b <- get @ByteString
    case fromByteString b of
      Nothing -> fail "string:String.Binary.get: encountered invalid utf-8 data"
      Just s -> pure s

-- binary, data, tojson, fromjson, lift, printfarg

byteLength :: String -> Int
{-# inline byteLength #-}
byteLength = coerce Bytes.length

charsLength :: String -> Int
{-# inline charsLength #-}
charsLength = coerce
  @(String -> Sum Int)
  @(String -> Int)
  (foldMap' (\_ -> Sum 1))

--graphemeLength :: String -> Int

append :: String -> String -> String
{-# inline append #-}
append = coerce
  @(Bytes -> Bytes -> Bytes)
  @(String -> String -> String)
  (<>)

empty :: String
{-# inline empty #-}
empty = coerce Bytes.empty

-----------
-- Folds --
-----------

foldr :: (Char -> b -> b) -> b -> String -> b
{-# inline foldr #-}
foldr f z0 = \s@(String (Bytes _ off0 len0)) ->
  let go !off !len
        | len == 0 = z0
        | otherwise = case next s off of
            Next bytesConsumed c -> f c (go (off + bytesConsumed) (len - bytesConsumed))
  in go off0 len0

foldl' :: (b -> Char -> b) -> b -> String -> b
{-# inline foldl' #-}
foldl' f z0 = \s@(String (Bytes _ off0 len0)) ->
  let go !acc !off !len
        | len == 0 = acc
        | otherwise = case next s off of
            Next bytesConsumed c -> go (f acc c) (off + bytesConsumed) (len - bytesConsumed)
  in go z0 off0 len0

foldrM :: Monad m => (Char -> b -> m b) -> b -> String -> m b
foldrM f z0 = \s@(String (Bytes _ off0 len0)) ->
  let go !off !len
        | len == 0 = pure z0
        | otherwise = case next s off of
            Next bytesConsumed c -> do
              f c =<< (go (off + bytesConsumed) (len - bytesConsumed))
  in go off0 len0

foldlM' :: Monad m => (b -> Char -> m b) -> b -> String -> m b
foldlM' f z0 = \s@(String (Bytes _ off0 len0)) ->
  let go !acc !off !len
        | len == 0 = pure acc
        | otherwise = case next s off of
            Next bytesConsumed c -> do
              b <- f acc c
              go b (off + bytesConsumed) (len - bytesConsumed)
  in go z0 off0 len0

foldMap :: (Monoid m) => (Char -> m) -> String -> m
{-# inline foldMap #-}
foldMap f = foldr ((<>) . f) mempty

foldMap' :: (Monoid m) => (Char -> m) -> String -> m
{-# inline foldMap' #-}
foldMap' f = foldl' (\acc a -> acc <> f a) mempty

--------------
-- Iterator --
--------------

-- (bytesConsumed, charAtPreviousIndex)
data Iter = Next {-# unpack #-} !Int {-# unpack #-} !Char

-- (unsafely) iterate through a 'String'
-- doesn't perform bounds checks. this should be done on
-- a per use-case basis
next :: String -> Int -> Iter
next (String bytes) i =
  let header = Bytes.unsafeIndex bytes i
  in if | iub8  header -> Next 1 (chr8 header)
        | iub16 header -> Next 2 (chr16 header (Bytes.unsafeIndex bytes (i + 1)))
        | iub24 header -> Next 3 (chr24 header (Bytes.unsafeIndex bytes (i + 1)) (Bytes.unsafeIndex bytes (i + 2)))
        | iub32 header -> Next 4 (chr32 header (Bytes.unsafeIndex bytes (i + 1)) (Bytes.unsafeIndex bytes (i + 2)) (Bytes.unsafeIndex bytes (i + 3)))
        | otherwise    -> error $ invalidCodePoint header i

invalidCodePoint :: Word8 -> Int -> [Char]
invalidCodePoint header index = "Invalid UTF-8 code point at position [" ++ show index ++ "]: " ++ showHex header ""

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
-- iubO :: Word8 -> Bool
-- iubO w = w .&. 0xC0 == 0x80
-- {-# inline iubO #-}

-----------------
-- Conversions --
-----------------

toList :: String -> [Char]
{-# inline toList #-}
toList xs = Exts.build (\c n -> foldr c n xs)

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

copyPtrToByteArray :: Ptr a -> MutableByteArray RealWorld -> Int -> Int -> IO ()
{-# inline copyPtrToByteArray #-}
copyPtrToByteArray (Ptr addr) (MutableByteArray arr) (I# off) (I# len)
  = IO $ \s0 -> case copyAddrToByteArray# addr arr off len s0 of
      s1 -> (# s1, () #)

foreign import ccall unsafe "validation.h run_utf8_validation"
  isUtf8Ptr :: Addr# -> Word64 -> Word64 -> Bool

foreign import ccall unsafe "validation.h run_utf8_validation"
  isUtf8ByteArray :: ByteArray# -> Word64 -> Word64 -> Bool

foreign import ccall unsafe "strlen"
  cstringLength# :: Addr# -> Int#

foreign import ccall unsafe "encode_utf8.h text_encode_utf8"
  c_encode_utf8 :: Ptr (Ptr Word8) -> ByteArray# -> CSize -> CSize -> IO ()


