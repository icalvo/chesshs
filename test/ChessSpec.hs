module ChessSpec (spec) where

import BoardElements (Color (..))
import Chess
import Names (e1, e8, f1, g1, h1, whiteKing)
import Plies
import Test.Hspec

spec :: Spec
spec = do
  describe "boardChanges" $ do
    it "records a simple move as one board change" $
      case boardChanges (Move whiteKing e1 e8) of
        [MoveBoardPiece src tgt] -> do
          src `shouldBe` e1
          tgt `shouldBe` e8
        _ -> expectationFailure "expected a single MoveBoardPiece"

    it "records white kingside castling as two moves" $
      case boardChanges (CastleKingSide White) of
        [MoveBoardPiece kingFrom kingTo, MoveBoardPiece rookFrom rookTo] -> do
          kingFrom `shouldBe` e1
          kingTo `shouldBe` g1
          rookFrom `shouldBe` h1
          rookTo `shouldBe` f1
        _ -> expectationFailure "expected king and rook moves"
