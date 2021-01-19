{-# language
    ImportQualifiedPost
  , NumericUnderscores
  , PackageImports
  , TemplateHaskell
  , ViewPatterns
#-}

module Typeclasses
  ( tests
  )
  where

import "hedgehog" Hedgehog
import "hedgehog" Hedgehog.Gen qualified as Gen
import "hedgehog" Hedgehog.Range qualified as Range
import "hedgehog-classes" Hedgehog.Classes
import "string" String qualified
import "this" Generators
import "base" GHC.Generics qualified as Generic

tests :: IO Bool
tests = lawsCheckOne
  Generators.string
  [ binaryLaws
  , eqLaws
  , ordLaws
  , semigroupLaws
  , monoidLaws
  , showLaws
  , showReadLaws
  , flip genericLaws (fmap Generic.from Generators.string)
  ]
