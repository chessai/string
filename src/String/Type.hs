module String.Type
  ( String(..)
  )
  where

import "base" Data.String (IsString(..))
import "base" Text.Show
import "bytebuild" Data.Bytes.Builder qualified as Builder
import "byteslice" Data.Bytes (Bytes)
import "byteslice" Data.Bytes.Chunks qualified as Chunks
import Prelude hiding (String)
import Prelude qualified

newtype String = String Bytes

deriving newtype instance Eq String
deriving newtype instance Ord String
deriving newtype instance Semigroup String
deriving newtype instance Monoid String

instance IsString String where
  fromString :: [Char] -> String
  {-# inline fromString #-}
  fromString xs = String
    $ Chunks.concat
    $ Builder.run 4080
    $ Prelude.foldMap Builder.char xs

{-
instance Show String where
  show :: String -> [Char]
  show s = id
    . showChar '\"'
    . showString (toList s)
    . showChar '\"'
    $ ""
-}
