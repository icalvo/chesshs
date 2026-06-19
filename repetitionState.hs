{-# LANGUAGE NoFieldSelectors #-}

module RepetitionState
  ( RepetitionStateClass (..),
    RepetitionState (..),
  )
where

import CoreTypes
import qualified Data.Map as Map

class RepetitionStateClass r where
  turn :: r -> Color
  whiteCastlingRights :: r -> CastlingRights
  blackCastlingRights :: r -> CastlingRights
  pawnCapturableEnPassant :: r -> Maybe Coordinate
  pieces :: r -> Map.Map (File, Rank) Piece

data RepetitionState = RepetitionState
  { turn :: Color,
    whiteCastlingRights :: CastlingRights,
    blackCastlingRights :: CastlingRights,
    pawnCapturableEnPassant :: Maybe Coordinate,
    pieces :: Map.Map (File, Rank) Piece
  }
  deriving (Eq, Show)

instance RepetitionStateClass RepetitionState where
  turn (RepetitionState t _ _ _ _) = t
  whiteCastlingRights (RepetitionState _ wcr _ _ _) = wcr
  blackCastlingRights (RepetitionState _ _ bcr _ _) = bcr
  pawnCapturableEnPassant (RepetitionState _ _ _ pcep _) = pcep
  pieces (RepetitionState _ _ _ _ pcs) = pcs
