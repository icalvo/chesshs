module Plies
  ( Ply (..),
    PlyOutput (..),
    PlyType (..),
  )
where

import CoreTypes

data PlyType
  = MoveType
  | CaptureType
  | EnPassantType
  | CastleKingSideType
  | CastleQueenSideType
  | MoveAndPromoteType
  | CaptureAndPromoteType
  deriving (Eq, Show)

-- A ply represents a piece move made by one of the players.
data Ply
  = Move Piece Coordinate Coordinate
  | Capture Piece Coordinate Coordinate
  | CaptureEnPassant Color Coordinate Coordinate
  | CastleKingSide Color
  | CastleQueenSide Color
  | Promote Color Coordinate Coordinate Shape
  | CaptureAndPromote Color Coordinate Coordinate Shape
  deriving (Eq, Show)

instance Colored Ply where
  color ply =
    case ply of
      Move p _ _ -> color p
      Capture p _ _ -> color p
      CaptureEnPassant c _ _ -> c
      CastleKingSide c -> c
      CastleQueenSide c -> c
      Promote c _ _ _ -> c
      CaptureAndPromote c _ _ _ -> c

-- A ply produces an output that can be a check, a checkmate, or none (regular ply).
data PlyOutput = PlyOutput
  { ply :: Ply,
    isCheck :: Bool,
    restOfPlies :: [Ply],
    drawOffer :: Bool
  }
  deriving (Eq, Show)

checkPly (ply, restOfPlies, drawOffer) =
  PlyOutput
    { ply = ply,
      restOfPlies = restOfPlies,
      isCheck = True,
      drawOffer = drawOffer
    }

regularPly (ply, restOfPlies, drawOffer) =
  PlyOutput
    { ply = ply,
      restOfPlies = restOfPlies,
      isCheck = False,
      drawOffer = drawOffer
    }
