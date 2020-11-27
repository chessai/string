{-# language
    BangPatterns
  , MagicHash
  , MultiWayIf
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , TypeFamilies
  , UnboxedTuples
#-}

module String where

import Data.Bits ((.&.), (.|.), unsafeShiftL)
import Data.Primitive.ByteArray
import GHC.Word (Word8(..), Word16(..), Word32(..))
import GHC.Exts hiding (build, toList)

import qualified GHC.Exts as Exts

import Prelude hiding (String, length, Foldable(..))

{- | api sketch
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

data String = String
  ByteArray# -- buffer
  Int#       -- offset
  Int#       -- length

--data MutableString s = MutableString
--  (MutableByteArray# s) -- buffer
--  Int#                  -- offset
--  Int#                  -- length

---------------
-- Instances --
---------------

instance Eq String where
  String buf0 off0 len0 == String buf1 off1 len1
    | neq len0 len1 = False
    | neq off0 off1 = False
    | sameByteArray# buf0 buf1 = True
    | otherwise =
        case compareByteArrays# buf0 off0 buf1 off1 len0 of
          0# -> True
          _  -> False
  {-# inline (==) #-}

instance Ord String where
  compare = todo

instance Semigroup String where
  (<>) = append
  {-# inline (<>) #-}

instance Monoid String where
  mempty = empty
  {-# inline mempty #-}

instance IsString String where
  fromString = String.fromList

instance IsList String where
  type Item String = Char
  fromListN = todo
  fromList  = todo
  toList    = toList

instance Show String where
  show s = "\"" ++ toList s ++ "\""

----------------
-- Public Api --
----------------

append :: String -> String -> String
append (String _ _ 0#) b = b
append a (String _ _ 0#) = a
append (String buf0 off0 len0) (String buf1 off1 len1) =
  let len = len0 +# len1
      buf = runByteArrayST $ \s0 ->
        case newByteArray# len s0 of
          (# s1, mb #) -> case copyByteArray# buf0 off0 mb 0# len0 s1 of
            s2 -> case copyByteArray# buf1 off1 mb len1 len1 s2 of
              s3 -> case unsafeFreezeByteArray# mb s3 of
                (# s4, b #) -> (# s4, b #)
  in String buf 0# len
{-# inline append #-}

empty :: String
empty = String (case mempty of { ByteArray b -> b }) 0# 0#
{-# inline empty #-}

toList :: String -> [Char]
toList xs = Exts.build (\c n -> foldr c n xs)
{-# inline toList #-}

fromList :: [Char] -> String
fromList cs = case byteArrayFromList cs of
  ByteArray b -> String b 0# (sizeofByteArray# b)
{-# inline fromList #-}

foldr :: (Char -> b -> b) -> b -> String -> b
foldr f z0 = \s@(String _ off len) ->
  let end = off +# len
      go ix
        | isTrue# (end ># ix) = case nextChar s ix of
            Iter ix' c -> f (C# c) (go ix')
        | otherwise = z0
  in go 0#
{-# inline foldr #-}

foldl' :: (b -> Char -> b) -> b -> String -> b
foldl' f z0 = \s@(String _ off len) ->
  let end = off +# len
      go ix !acc
        | isTrue# (ix <# end) = case nextChar s ix of
            Iter ix' c -> go ix' (f acc (C# c))
        | otherwise = acc
  in go 0# z0
{-# inline foldl' #-}

data Iter = Iter
  Int#  -- next byte to index
  Char# -- char at the previous byte index

instance Show Iter where
  show (Iter i# c#) = "Iter " ++ show (I# i#) ++ " " ++ show (C# c#)

nextChar :: String -> Int# -> Iter
nextChar (String buf off _) i =
  case indexWord8Array# buf (off +# i) of
    w0 -> let w = W8# w0
          in if | iub8  w -> let c = chr8 w0
                             in Iter (i +# 1#) c
                | iub16 w -> let w1 = indexWord8Array# buf (off +# i +# 1#)
                                 c  = chr16 w0 w1
                             in Iter (i +# 2#) c
                | iub24 w -> let w1 = indexWord8Array# buf (off +# i +# 1#)
                                 w2 = indexWord8Array# buf (off +# i +# 2#)
                                 c  = chr24 w0 w1 w2
                             in Iter (i +# 3#) c
                | iub32 w -> let w1 = indexWord8Array# buf (off +# i +# 1#)
                                 w2 = indexWord8Array# buf (off +# i +# 2#)
                                 w3 = indexWord8Array# buf (off +# i +# 3#)
                                 c  = chr32 w0 w1 w2 w3
                             in Iter (i +# 4#) c
                | otherwise -> error $ "invalid UTF-8 header byte: " ++ show w
{-# noinline nextChar #-}

-------------
-- Helpers --
-------------

sameByteArray# :: ByteArray# -> ByteArray# -> Bool
sameByteArray# b0# b1# =
  case sameMutableByteArray# (unsafeCoerce# b0#) (unsafeCoerce# b1#) of
    r -> isTrue# r
{-# inline sameByteArray# #-}

neq :: Int# -> Int# -> Bool
neq x y = case x ==# y of { 0# -> True; _ -> False; }
{-# inline neq #-}

runByteArrayST :: (forall s. State# s -> (# State# s, ByteArray# #)) -> ByteArray#
runByteArrayST f = runRW# (\s0 -> case f s0 of { (# _, r #) -> r })
{-# noinline runByteArrayST #-}

todo :: a
todo = error "TODO"
{-# noinline todo #-}

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

chr8 :: Word# -> Char#
chr8 w = chr# (word2Int# w)

chr16 :: Word# -> Word# -> Char#
chr16 w0# w1# =
  let w0 = w8_2_w16 (W8# w0#)
      w1 = w8_2_w16 (W8# w1#)
  in case unsafeShiftL w0 8 .|. w1 of
       W16# w -> chr# (word2Int# w)

chr24 :: Word# -> Word# -> Word# -> Char#
chr24 w0# w1# w2# =
  let w0 = w8_2_w32 (W8# w0#)
      w1 = w8_2_w32 (W8# w1#)
      w2 = w8_2_w32 (W8# w2#)
  in case unsafeShiftL w0 16 .|. unsafeShiftL w1 8 .|. w2 of
       W32# w -> chr# (word2Int# w)

chr32 :: Word# -> Word# -> Word# -> Word# -> Char#
chr32 w0# w1# w2# w3# =
  let w0 = w8_2_w32 (W8# w0#)
      w1 = w8_2_w32 (W8# w1#)
      w2 = w8_2_w32 (W8# w2#)
      w3 = w8_2_w32 (W8# w3#)
  in case unsafeShiftL w0 24 .|. unsafeShiftL w1 16 .|. unsafeShiftL w2 8 .|. w3 of
       W32# w -> chr# (word2Int# w)

w8_2_w16 :: Word8 -> Word16
w8_2_w16 = fromIntegral

w8_2_w32 :: Word8 -> Word32
w8_2_w32 = fromIntegral
