{-# LANGUAGE DisambiguateRecordFields #-}

module Chess where

import CoreTypes
import Names (a1, a8, c1, c8, d1, d8, e1, e8, f1, f8, g1, g8, h1, h8)
import Plies

data BoardChange
  = MoveBoardPiece Coordinate Coordinate
  | RemoveBoardPiece Coordinate
  | AddBoardPiece Color Shape Coordinate
  deriving (Eq)

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
