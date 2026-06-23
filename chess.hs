{-# LANGUAGE DisambiguateRecordFields #-}

module Chess where

import CoreTypes
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import Plies

a -. b = b . a

placedPiece color shape pos = (pos, Piece color shape)

opposite color =
  case color of
    White -> Black
    Black -> White

type Square = (Coordinate, Maybe Piece)

isEmpty = snd -. isNothing

hasPiece = snd -. isJust

pieceColor :: (Colored b) => (a, Maybe b) -> Maybe Color
pieceColor = snd -. fmap color

piece = snd

pieceCoord = fst

squareAt :: Coordinate -> Map.Map Coordinate Piece -> Square
squareAt coord placedPieces = (coord, Map.lookup coord placedPieces)

placedPieces @@ coord = squareAt coord placedPieces

data BoardChange
  = MoveBoardPiece Coordinate Coordinate
  | RemoveBoardPiece Coordinate
  | AddBoardPiece Color Shape Coordinate
  deriving (Eq)

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

boardChanges :: Ply -> [BoardChange]
boardChanges ply =
  case ply of
    Move _ source target ->
      [ MoveBoardPiece source target
      ]
    Capture _ source target ->
      [ RemoveBoardPiece target,
        MoveBoardPiece source target
      ]
    CaptureEnPassant _ source@(Coordinate (_, sr)) target@(Coordinate (tf, _)) ->
      [ let coordOfCapturedPawn = Coordinate (tf, sr)
         in RemoveBoardPiece coordOfCapturedPawn,
        MoveBoardPiece source target
      ]
    CastleKingSide White ->
      [ MoveBoardPiece e1 g1,
        MoveBoardPiece h1 f1
      ]
    CastleQueenSide White ->
      [ MoveBoardPiece e1 c1,
        MoveBoardPiece a1 d1
      ]
    CastleKingSide Black ->
      [ MoveBoardPiece e8 g8,
        MoveBoardPiece h8 f8
      ]
    CastleQueenSide Black ->
      [ MoveBoardPiece e8 c8,
        MoveBoardPiece a8 d8
      ]
    Promote color source target shape ->
      [ RemoveBoardPiece source,
        AddBoardPiece color shape target
      ]
    CaptureAndPromote color source target shape ->
      [ RemoveBoardPiece source,
        AddBoardPiece color shape target
      ]

coordOfCapturedPiece ply =
  case ply of
    Capture _ _ target -> Just target
    CaptureAndPromote _ _ target _ -> Just target
    CaptureEnPassant _ (Coordinate (_, sourceRank)) (Coordinate (targetFile, _)) -> Just (Coordinate (targetFile, sourceRank))
    _ -> Nothing

sameColor (Piece sourceColor _) square =
  case pieceColor square of
    Just col -> col == sourceColor
    Nothing -> False

differentColor p s = not (sameColor p s)
