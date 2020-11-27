{-# LANGUAGE OverloadedStrings #-}

{-# options_ghc -O2 #-} -- -ddump-simpl -dsuppress-all #-}

module Main (main) where

import Prelude hiding (String)
import String (String)
import qualified String

main :: IO ()
main = do
  putStrLn $ show $ String.fromList "1234342342" <> String.fromList "23498230489230489230840"
