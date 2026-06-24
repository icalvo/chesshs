module Plies where

import BoardElements
import Names (c1, c8, e1, e8, g1, g8)

-- A ply represents a piece move made by one of the players.
data Ply
  = Move Piece Coordinate Coordinate
  | Capture Piece Coordinate Coordinate
  | CaptureEnPassant Color Coordinate Coordinate
  | CastleKingSide Color
  | CastleQueenSide Color
  | Promote Color Coordinate Coordinate Shape
  | CaptureAndPromote Color Coordinate Coordinate Shape
  deriving (Eq)

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
  deriving (Eq)

data PlyType
  = MoveType
  | CaptureType
  | EnPassantType
  | CastleKingSideType
  | CastleQueenSideType
  | MoveAndPromoteType
  | CaptureAndPromoteType
  deriving (Eq)

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

plyType ply =
  case ply of
    Move {} -> MoveType
    Capture {} -> CaptureType
    CaptureEnPassant {} -> EnPassantType
    CastleKingSide _ -> CastleKingSideType
    CastleQueenSide _ -> CastleQueenSideType
    Promote {} -> MoveAndPromoteType
    CaptureAndPromote {} -> CaptureAndPromoteType

isCapture ply =
  case ply of
    Capture {} -> True
    CaptureAndPromote {} -> True
    CaptureEnPassant {} -> True
    _ -> False

source ply =
  case ply of
    Move _ source _ -> source
    Capture _ source _ -> source
    CaptureEnPassant _ source _ -> source
    CastleKingSide White -> e1
    CastleQueenSide White -> e1
    CastleKingSide Black -> e8
    CastleQueenSide Black -> e8
    Promote _ source _ _ -> source
    CaptureAndPromote _ source _ _ -> source

target ply =
  case ply of
    Move _ _ target -> target
    Capture _ _ target -> target
    CaptureEnPassant _ _ target -> target
    CastleKingSide White -> g1
    CastleQueenSide White -> c1
    CastleKingSide Black -> g8
    CastleQueenSide Black -> c8
    Promote _ _ target _ -> target
    CaptureAndPromote _ _ target _ -> target

shape ply =
  case ply of
    Move (Piece _ shape) _ _ -> shape
    Capture (Piece _ shape) _ _ -> shape
    CaptureEnPassant {} -> Pawn
    CastleKingSide _ -> King
    CastleQueenSide _ -> King
    Promote {} -> Pawn
    CaptureAndPromote {} -> Pawn
