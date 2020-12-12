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
  , TypeFamilies
  , UnboxedTuples
  , ViewPatterns
#-}

module String where

import Data.Bits ((.&.))
import Numeric (showHex)
import Data.Bytes.Types (Bytes(..))
import Data.Primitive.ByteArray
import Debug.Trace
import GHC.Exts hiding (build, toList)
import GHC.Word (Word8(..), Word16(..), Word32(..))
import Prelude hiding (String, length, Foldable(..))
import qualified Data.Bytes as Bytes
import qualified Data.List as List
import qualified GHC.Exts as Exts
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.Bytes.Builder as Builder
import qualified Prelude

{-
-- querying
byteLength      :: String -> Int
codepointLength :: String -> Int
graphemeLength  :: String -> Int

-- conversions
fromAscii :: PrimArray Word8 -> Maybe String
fromUtf8  :: PrimArray Word8 -> Maybe String
fromUtf16 :: PrimArray Word8 -> Maybe String
fromUtf32 :: PrimArray Word8 -> Maybe String

fromAsciiLossy :: PrimArray Word8 -> String
fromUtf8Lossy  :: PrimArray Word8 -> String
fromUtf16Lossy :: PrimArray Word8 -> String
fromUtf32Lossy :: PrimArray Word8 -> String
-}

-----------
-- Types --
-----------

newtype String = String Bytes

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

append :: String -> String -> String
append (String x) (String y) = String (x <> y)
{-# inline append #-}

empty :: String
empty = String Bytes.empty
{-# inline empty #-}

toList :: String -> [Char]
toList xs = Exts.build (\c n -> foldr c n xs)
{-# inline toList #-}

fromList :: [Char] -> String
fromList xs = String
  $ Chunks.concat
  $ Builder.run 4080
  $ Prelude.foldMap Builder.char xs

foldr :: (Char -> b -> b) -> b -> String -> b
foldr f z0 = \s@(String (Bytes _ off0 len0)) ->
  let go !off !len
        | len == 0 = z0
        | otherwise = case traceShowId (nextChar s off) of
            Next bytesConsumed c -> f c (go (off + bytesConsumed) (len - bytesConsumed))
  in go off0 len0
{-# inline foldr #-}

foldl' :: (b -> Char -> b) -> b -> String -> b
foldl' f z0 = \s@(String (Bytes _ off0 len0)) ->
  let go !acc !off !len
        | len == 0 = acc
        | otherwise = case nextChar s off of
            Next bytesConsumed c -> go (f acc c) (off + bytesConsumed) (len - bytesConsumed)
  in go z0 off0 len0
{-# inline foldl' #-}

data Iter = Next Int Char
  deriving (Show)

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
