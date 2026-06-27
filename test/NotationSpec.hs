module NotationSpec (spec) where

import Arbitrary ()
import BoardElements
import Chess (coordOfCapturedPiece)
import Names (e4, whiteQueen)
import Notation
import Plies
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = do
  describe "pgn" $ do
    prop "renders a coordinate as a lowercase file letter and rank digit" $ \c@(Coordinate _) ->
      let s = pgn c
       in property $
            length s == 2
              && head s `elem` ['a' .. 'h']
              && last s `elem` ['1' .. '8']

    prop "renders e4 as expected" $
      pgn e4 === "e4"

  describe "coordOfCapturedPiece" $ do
    prop "returns the capture square for capture plies" $ \target ->
      coordOfCapturedPiece (Capture whiteQueen e4 target) === Just target
