module String where

import "base" Data.Monoid (Sum(..))
import "this" String.Folds qualified as Folds
import "this" String.Type
import Prelude hiding (String)
import Prelude qualified

byteLength :: String -> Int
{-# inline byteLength #-}
byteLength = coerce Bytes.length

charsLength :: String -> Int
{-# inline charsLength #-}
charsLength = getSum . Folds.foldMap (\_ -> Sum 1)

--graphemeLength :: String -> Int

append :: String -> String -> String
{-# inline append #-}
append (String x) (String y) = String (x <> y)

empty :: String
{-# inline empty #-}
empty = String Bytes.empty
