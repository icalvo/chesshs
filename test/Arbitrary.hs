{-# OPTIONS_GHC -Wno-orphans #-}

module Arbitrary where

import BoardElements
import Test.QuickCheck

instance Arbitrary File where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Rank where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Coordinate where
  arbitrary = Coordinate <$> ((,) <$> arbitrary <*> arbitrary)
