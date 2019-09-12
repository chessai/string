{-# language
    BangPatterns
  , MagicHash
  , RankNTypes
  , UnboxedTuples
  #-}


module String.Ascii
  ( String

  , empty
  , append

  , foldr
  , foldl'

  , Builder
  , build
  ) where

import Control.Monad.ST (runST)
import Data.Primitive.ByteArray
import Data.Primitive.Types
import GHC.Exts hiding (build, compareByteArrays#, toList)

--import qualified Data.Char as Char
import qualified Data.List as List
import qualified GHC.Exts as Exts

import Prelude hiding (String, length, Foldable(..))

import String.Builder (BI(..))
import qualified String.Builder as SB

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

-- Slices
data Slice
-}

data String = String ByteArray#

instance Eq String where
  String b0# == String b1#
    | sameByteArray# b0# b1# = True
    | lengthNeq b0# b1# = False
    | otherwise = compareByteArrays# b0# b1# (sizeofByteArray# b0#) == EQ
  {-# inline (==) #-}

instance Semigroup String where
  (<>) = append
  {-# inline (<>) #-}

instance Monoid String where
  mempty = empty
  {-# inline mempty #-}

instance IsString String where
  fromString = build . fromString

instance Show String where
  show s = "\"" ++ toList s ++ "\""

length :: String -> Int
length (String b#) = I# (sizeofByteArray# b#)
{-# inline length #-}

index :: String -> Int -> Char
index (String b#) (I# i#) = SB.w2c (indexByteArray# b# i#)
{-# inline index #-}

toList :: String -> [Char]
toList xs = Exts.build (\c n -> foldr c n xs)
{-# inline toList #-}

foldr :: (Char -> b -> b) -> b -> String -> b
foldr f z0 = \s ->
  let
    !sz = length s
    go !ix = if sz > ix
      then f (index s ix) (go (ix + 1))
      else z0
  in go 0
{-# inline foldr #-}

foldl' :: (b -> Char -> b) -> b -> String -> b
foldl' f z0 = \s ->
  let
    !sz = length s
    go !ix !acc = if ix < sz
      then go (ix + 1) (f acc (index s ix))
      else acc
  in go 0 z0
{-# inline foldl' #-}

empty :: String
empty = build mempty
{-# inline empty #-}

append :: String -> String -> String
append (String x) (String y) =
  let
    !(ByteArray b#) = runST $ do
      let len_x = I# (sizeofByteArray# x)
      let len_y = I# (sizeofByteArray# y)
      let len = len_x + len_y
      buf <- newByteArray len
      copyByteArray buf 0 (ByteArray x) 0 len_x
      copyByteArray buf len_x (ByteArray y) 0 len_y
      unsafeFreezeByteArray buf
  in String b#
{-# inline append #-}

sameByteArray# :: ByteArray# -> ByteArray# -> Bool
sameByteArray# b0# b1# =
  case sameMutableByteArray# (unsafeCoerce# b0#) (unsafeCoerce# b1#) of
    r -> isTrue# r
{-# inline sameByteArray# #-}

lengthNeq :: ByteArray# -> ByteArray# -> Bool
lengthNeq b0# b1# =
  case sizeofByteArray# b0# ==# sizeofByteArray# b1# of
    0# -> True
    _  -> False
{-# inline lengthNeq #-}

compareByteArrays# :: ByteArray# -> ByteArray# -> Int# -> Ordering
compareByteArrays# b0# b1# n# =
  compare (I# (Exts.compareByteArrays# b0# 0# b1# 0# n#)) 0
{-# inline compareByteArrays# #-}

newtype Builder = Builder (forall s. BI s)

instance Semigroup Builder where
  Builder x <> Builder y = Builder (x <> y)
  {-# inline (<>) #-}

instance Monoid Builder where
  mempty = Builder mempty
  {-# inline mempty #-}

instance IsString Builder where
  fromString = List.foldl' go mempty
    where
      go (Builder b) c = Builder (b <> SB.buildAsciiChar c)
  {-# inline fromString #-}

build :: Builder -> String
build (Builder b) = String (SB.build b)
