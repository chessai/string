{-# language
    BangPatterns
  , DerivingStrategies
  , MagicHash
  , MonoLocalBinds
  , RankNTypes
  , StandaloneDeriving
  , UnboxedTuples
  #-}


module String.Ascii
  ( -- * Types
    -- ** Unsliced Strings
    String
    -- ** Sliced Strings
  , Slice
    -- ** Builders
  , Builder

  , empty
  , append
  , length
  , foldr
  , foldl'
  , asByteArray
  , toSlice
  , unsafeToSlice
  ) where

import Control.Monad.ST (runST)
import Control.Monad.Primitive (primitive_)
import Control.Monad.Primitive.Convenience (MonadPrim)
import Data.Primitive.ByteArray
import Data.Primitive.Types
import Data.Word (Word8)
import GHC.Exts hiding (build, compareByteArrays#, toList)

import qualified Data.Char as Char
import qualified Data.List as List
import qualified GHC.Base as Base
import qualified GHC.Exts as Exts

import Prelude hiding (String, length, Foldable(..))

import qualified Builder as B

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

-----------
-- Types --
-----------

data String = String ByteArray#

data Slice = Slice
  {-# UNPACK #-} !String -- buffer
  {-# UNPACK #-} !Int    -- offset
  {-# UNPACK #-} !Int    -- length

data MutableString s = MutableString (MutableByteArray# s)

data MutableSlice s = MutableSlice
  {-# UNPACK #-} !(MutableString s) -- buffer
  {-# UNPACK #-} !Int               -- offset
  {-# UNPACK #-} !Int               -- length

---------------
-- Instances --
---------------

instance Eq String where
  String b0# == String b1#
    | sameByteArray# b0# b1# = True
    | lengthNeq b0# b1# = False
    | otherwise = compareByteArrays# b0# b1# (sizeofByteArray# b0#) == EQ
  {-# inline (==) #-}

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

deriving stock instance Eq Slice

instance Semigroup Slice where
  (<>) = appendSlice
  {-# inline (<>) #-}

instance Monoid Slice where
  mempty = emptySlice
  {-# inline mempty #-}

---------------
-- Functions --
---------------

-- | Convert a 'String' to a 'Slice', without copying.
--   This function is unsafe because it does not check
--   the validity of the length and offset.
unsafeToSlice :: ()
  => String -- ^ String
  -> Int    -- ^ Offset
  -> Int    -- ^ Length
  -> Slice
unsafeToSlice = Slice

-- | Convert a 'String' to a 'Slice', without copying.
--   Returns 'Nothing' if the length and offset are invalid.
--
--   The length (len) and offset (off) are valid if, for some
--   @str :: 'String'@:
--
--   @off < len && len <= length str@
--
toSlice :: ()
  => String
  -> Int
  -> Int
  -> Maybe Slice
toSlice s off len
  | off < len && len <= length s = Just (unsafeToSlice s off len)
  | otherwise = Nothing

-- | Copy a slice of a 'MutableString' into another
--   'MutableString'. The two slices may not overlap.
copyMutable :: MonadPrim s m
  => MutableString s -- ^ destination buffer
  -> Int -- ^ offset into destination buffer
  -> MutableString s -- ^ source buffer
  -> Int -- ^ offset into source buffer
  -> Int -- ^ number of characters to copy
  -> m ()
copyMutable (MutableString dst#) (I# doff#)
            (MutableString src#) (I# soff#) (I# sz#)
  = primitive_
      (copyMutableByteArray# src# soff# dst# doff# sz#)

copy :: MonadPrim s m
  => MutableString s -- ^ destination array
  -> Int -- ^ offset into destination array
  -> String -- ^ source string
  -> Int -- ^ offset into source string
  -> Int -- ^ number of characters to copy
  -> m ()
copy (MutableString dst#) (I# doff#)
     (String        src#) (I# soff#) (I# sz#)
  = primitive_
      (copyByteArray# src# soff# dst# doff# sz#)

asSlice :: String -> (Slice -> a) -> a
asSlice s f = f (unsafeToSlice s 0 (length s))

asByteArray :: String -> (ByteArray -> a) -> a
asByteArray s f = f (toByteArray s)

-- | Initialise a 'MutableString' with the given
--   capacity. None of the characters are initialised.
new :: MonadPrim s m => Int -> m (MutableString s)
new sz = do
  !(MutableByteArray marr#) <- newByteArray sz
  pure (MutableString marr#)

asMutableString :: MonadPrim s m => String -> (MutableString s -> m a) -> m a
asMutableString s f = do
  buffer <- new (length s)
  copy buffer 0 s 0 (length s)
  f buffer

--asMutableSlice :: MonadPrim s m => String -> (MutableSlice s -> m a) -> m a
--asMutableSlice s f = do
--  buffer <- new (length s)

appendSlice :: Slice -> Slice -> Slice
appendSlice (Slice _ _ 0) b = b
appendSlice a (Slice _ _ 0) = a
appendSlice (Slice s0 off0 len0) (Slice s1 off1 len1) =
  let
    !len = len0 + len1
    !(ByteArray b#) = runST $ do
      buf <- newByteArray len
      copyByteArray buf 0 (toByteArray s0) off0 len0
      copyByteArray buf len1 (toByteArray s1) off1 len1
      unsafeFreezeByteArray buf
  in Slice (String b#) 0 len
{-# inline appendSlice #-}

emptySlice :: Slice
emptySlice = Slice mempty 0 0
{-# inline emptySlice #-}

length :: String -> Int
length (String b#) = I# (sizeofByteArray# b#)
{-# inline length #-}

index :: String -> Int -> Char
index (String b#) (I# i#) = w2c (indexByteArray# b# i#)
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

newtype Builder = Builder B.Builder

instance Semigroup Builder where
  Builder x <> Builder y = Builder (x <> y)
  {-# inline (<>) #-}

instance Monoid Builder where
  mempty = Builder mempty
  {-# inline mempty #-}

instance IsString Builder where
  fromString = List.foldl' go mempty
    where
      go (Builder b) c = Builder (b <> buildAsciiChar c)
  {-# inline fromString #-}

build :: Builder -> String
build (Builder b) = case B.build b of
  ByteArray b# -> String b#
{-# inline build #-}

buildAsciiChar :: Char -> B.Builder
buildAsciiChar = B.word8 . c2w
{-# inline buildAsciiChar #-}

w2c :: Word8 -> Char
w2c = Base.unsafeChr . fromIntegral
{-# inline w2c #-}

c2w :: Char -> Word8
c2w = fromIntegral . Char.ord
{-# inline c2w #-}

toByteArray :: String -> ByteArray
toByteArray (String b#) = ByteArray b#
{-# inline toByteArray #-}

