{-# language
    OverloadedStrings
  , TypeApplications
#-}

{-# options_ghc -O2 #-}

module Main (main) where

import Prelude hiding (String)
import String (String)
import qualified String

main :: IO ()
main = do
  putStrLn $ show @String $ "123" <> "456"
  putStrLn $ show $ map String.isUtf8 $ ["123", "456"]
