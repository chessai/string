{-# language
    BangPatterns
  , MagicHash
  , RankNTypes
  , UnboxedTuples
  #-}

module String.Builder
  ( BI(..)
  , build
  , buildAsciiChar

  , Writer(..)
  , writeUnaligned
  , writeAsciiChar

  , c2w
  , w2c
  ) where

import Data.Primitive (Prim(..))
import Data.Primitive.ByteArray
import Data.Primitive.ByteArray.Unaligned
import Data.Semigroup (Semigroup(..))
import Data.Word (Word8)
import GHC.Exts hiding (build)

import qualified Data.Char as Char
import qualified GHC.Base as Base

import Prelude hiding (String)

newtype Writer s = K
  (    MutableByteArray# s -- ^ buffer
    -> Int# -- ^ offset into the buffer
    -> (State# s -> (# State# s, Int# #)) -- ^ update to offset
  )

instance Semigroup (Writer s) where
  K x <> K y = K $ \marr# ix0# s0# -> case x marr# ix0# s0# of
    (# s1#, ix1# #) -> y marr# ix1# s1#
  {-# inline (<>) #-}

instance Monoid (Writer s) where
  mempty = K $ \_ ix0# s0# -> (# s0#, ix0# #)
  {-# inline mempty #-}

-- Internal Builder
data BI s = BI
  !Int -- ^ length
  !(Writer s) -- ^ accumulated writer actions

instance Semigroup (BI s) where
  BI i w <> BI i' w' = BI (i + i') (w <> w')
  {-# inline (<>) #-}

instance Monoid (BI s) where
  mempty = BI 0 mempty
  {-# inline mempty #-}

writeUnaligned :: (Prim a, PrimUnaligned a)
  => a
  -> Writer s
writeUnaligned a = K $ \marr# ix0# s0# ->
  case writeUnalignedByteArray# marr# ix0# a s0# of
    s1# -> (# s1#, ix0# +# alignment# a #)
{-# inline writeUnaligned #-}

writeAsciiChar :: Char -> Writer s
writeAsciiChar = writeUnaligned . c2w
{-# inline writeAsciiChar #-}

buildAsciiChar :: Char -> BI s
buildAsciiChar = BI 1 . writeAsciiChar
{-# inline buildAsciiChar #-}

c2w :: Char -> Word8
c2w = fromIntegral . Char.ord
{-# inline c2w #-}

w2c :: Word8 -> Char
w2c = Base.unsafeChr . fromIntegral
{-# inline w2c #-}

runWriter# :: Int# -> Writer s -> State# s -> (# State# s, ByteArray# #)
runWriter# sz# (K g) = \s0# -> case newByteArray# sz# s0# of
  (# s1#, marr# #) -> case g marr# 0# s1# of
    (# s2#, _ #) -> case unsafeFreezeByteArray# marr# s2# of
      (# s3#, b# #) -> (# s3#, b# #)
{-# inline runWriter# #-}

build :: (forall s. BI s) -> ByteArray#
build (BI (I# len#) w) = case runRW# (runWriter# len# w) of
  (# _, b# #) -> b#
{-# inline build #-}
