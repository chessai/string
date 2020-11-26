{-# language
    BangPatterns
  , DerivingStrategies
  , MagicHash
  , MonoLocalBinds
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeApplications
  , UnboxedTuples
#-}

module String
  ( String
  , append
  , empty
  ) where

import GHC.ST (ST(..), runST)
import Control.Monad.Primitive (primitive_)
import Data.Monoid (All(..))
import Data.Bits (Bits((.&.)))
import Data.Primitive.ByteArray
import Data.Primitive.Types
import Data.Word (Word8,Word64)
import GHC.Exts hiding (build, toList)

import qualified Data.Char as Char
import qualified Data.List as List
import qualified GHC.Base as Base
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

data MutableString s = MutableString
  (MutableByteArray# s) -- buffer
  Int#                  -- offset
  Int#                  -- length

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

sameByteArray# :: ByteArray# -> ByteArray# -> Bool
sameByteArray# b0# b1# =
  case sameMutableByteArray# (unsafeCoerce# b0#) (unsafeCoerce# b1#) of
    r -> isTrue# r
{-# inline sameByteArray# #-}

neq :: Int# -> Int# -> Bool
neq x y = case x ==# y of { 0# -> True; _ -> False; }
{-# inline neq #-}

append :: String -> String -> String
append (String _ _ 0#) b = b
append a (String _ _ 0#) = a
append (String buf0 off0 len0) (String buf1 off1 len1) =
  let len = len0 +# len1
      ByteArray buf = runByteArrayST $ ST $ \s0 ->
        case newByteArray# len s0 of
          (# s1, buf #) -> case copyByteArray# buf0 off0 buf 0# len0 s1 of
            s2 -> case copyByteArray# buf1 off1 buf len1 len1 s2 of
              s3 -> case unsafeFreezeByteArray# buf s3 of
                (# s4, b #) -> (# s4, ByteArray b #)
  in String buf 0# len
{-# inline append #-}

empty :: String
empty = String (case mempty of { ByteArray b -> b }) 0# 0#

runByteArrayST :: (forall s. ST s ByteArray) -> ByteArray
runByteArrayST f = ByteArray (runRW# (\s0 -> case f of { ST g -> case g s0 of { (# _, ByteArray r #) -> r }}))
{-# inline runByteArrayST #-}


{-
instance Ord String where
  compare b0 b1 = compare (toByteArray b0) (toByteArray b1)

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
-}

---------------
-- Functions --
---------------

{-
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

unsafeFromByteArray :: ByteArray -> String
unsafeFromByteArray (ByteArray b#) = String b#
{-# inline unsafeFromByteArray #-}

fromByteArray :: ByteArray -> Maybe String
fromByteArray b@(ByteArray b#) = if isAscii b
  then Just (String b#)
  else Nothing
{-# inline fromByteArray #-}

-- naive, should be improved
isAscii :: ByteArray -> Bool
isAscii b = go 0
  where
    len = sizeofByteArray b
    go !ix = if ix < len
      then case indexByteArray b ix of
        w -> if w < m8 then go (ix + 1) else False
      else True

-- | Binary:
--     1000000010000000100000001000000010000000100000001000000010000000
--   Decimal:
--     9259542123273814144
m64 :: Word64
{-# inline m64 #-}
m64 = 0x8080808080808080

-- | Binary:
--     10000000
--   Decimal:
--     128
m8 :: Word8
{-# inline m8 #-}
m8 = 0x80

{-
isAsciiPtrW64 :: ByteArray -> Int -> Int -> Bool
isAsciiPtrW64 b !p !q
  | p == q = True
  | otherwise = case indexByteArray b p of
      (w :: Word64) -> if isAsciiW64 w
        then isAsciiPtrW64 b (p + 8) q
        else False

isAsciiPtrW8 :: ByteArray -> Int -> Int -> Bool
isAsciiPtrW8 b !p !q
  | p == q = True
  | otherwise = case indexByteArray b p of
      (w :: Word8) -> if isAsciiW8 w
        then isAsciiPtrW8 b (p + 1) q
        else False



isAsciiW64 :: Word64 -> Bool
isAsciiW64 w = w .&. m64 == 0

isAsciiW8 :: Word8 -> Bool
isAsciiW8 w = w .&. m8 == 0

isAsciiSmall :: ByteArray -> Int -> Int -> Bool
isAsciiSmall b off len = go off
  where
    go !ix = if ix < len
      then case indexByteArray b ix of
        w -> if w < m8 then go (ix + 1) else False
      else True

alignPtrPos :: Int -> Int
alignPtrPos !(I# i#) = I# (case remInt# i# 8# of
  0# -> i#
  n# -> i# +# (8# -# n#))

alignPtrNeg :: Int -> Int
alignPtrNeg !(I# i#) = I# (case remInt# i# 8# of
  0# -> i#
  n# -> i# +# (negateInt# n#))
{-# inline alignPtrNeg #-}

isAscii :: ByteArray -> Int -> Int -> Bool
isAscii b off len
  | len < 8 = isAsciiSmall b off len
  | otherwise =
      let
        startPre, endPre, startMid, endMid, startPost, endPost :: Int
        startPre  = off
        endPre    = alignPtrPos startPre
        startMid  = endPre
        endMid    = startPost
        startPost = alignPtrNeg endPost
        endPost   = off + len

        startIsAscii = isAsciiPtrW8 b startPre endPre
      in if startIsAscii
        then
          let endIsAscii = isAsciiPtrW8 b startPost endPost
          in if endIsAscii
            then isAsciiPtrW64 b startMid endMid
            else False
        else False
-}
-}
