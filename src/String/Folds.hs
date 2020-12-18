module String.Folds
  ( foldr
  , foldl'
  , foldMap
  , foldMap'
  ) where

import Prelude hiding
  ( String, foldr, foldMap
#if MIN_VERSION_base(4,15,0)
  , foldMap'
#endif
  )
import "byteslice" Data.Bytes.Types (Bytes(..))
import "this" String.Iterate
import "this" String.Type

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

foldMap :: (Monoid m) => (Char -> m) -> String -> m
{-# inline foldMap #-}
foldMap f = foldr ((<>) . f) mempty

foldMap' :: (Monoid m) => (Char -> m) -> String -> m
{-# inline foldMap' #-}
foldMap' f = foldl' (\acc a -> acc <> f a) mempty
