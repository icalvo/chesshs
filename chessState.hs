{-# LANGUAGE DisambiguateRecordFields #-}

module ChessState
  ( ChessState (..),
    RepetitionStateClass (..),
    RepetitionState (..),
    repState,
    initialGameState,
  )
where

import CoreTypes
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Plies
import RepetitionState

a -. b = b . a

-- In chess, the state of the game is defined by several properties in addition
-- to the pieces on the board. We need to know the castling rights of each player,
-- whether the last move allows en passant capture (and the coordinate of the pawn that
-- can be captured), the repeated coordinates and their number, and the number of
-- moves since a pawn moved or a capture happened.
data ChessState = ChessState
  { plies :: [PlyOutput],
    pliesWithoutPawnOrCapture :: Int,
    repeatableStates :: NonEmpty RepetitionState,
    numberOfMoves :: Int
  }
  deriving (Eq, Show)

repState = repeatableStates -. NE.head

instance RepetitionStateClass ChessState where
  turn = repState -. turn
  whiteCastlingRights = repState -. whiteCastlingRights
  blackCastlingRights = repState -. blackCastlingRights
  pawnCapturableEnPassant = repState -. pawnCapturableEnPassant
  pieces = repState -. pieces

initialGameState =
  let initialPieces =
        Map.fromList
          [ (a1, whiteRook),
            (b1, whiteKnight),
            (c1, whiteBishop),
            (d1, whiteQueen),
            (e1, whiteKing),
            (f1, whiteBishop),
            (g1, whiteKnight),
            (h1, whiteRook),
            (a2, whitePawn),
            (b2, whitePawn),
            (c2, whitePawn),
            (d2, whitePawn),
            (e2, whitePawn),
            (f2, whitePawn),
            (g2, whitePawn),
            (h2, whitePawn),
            (a7, blackPawn),
            (b7, blackPawn),
            (c7, blackPawn),
            (d7, blackPawn),
            (e7, blackPawn),
            (f7, blackPawn),
            (g7, blackPawn),
            (h7, blackPawn),
            (a8, blackRook),
            (b8, blackKnight),
            (c8, blackBishop),
            (d8, blackQueen),
            (e8, blackKing),
            (f8, blackBishop),
            (g8, blackKnight),
            (h8, blackRook)
          ]
   in ChessState
        { repeatableStates =
            NE.singleton
              RepetitionState
                { turn = White,
                  whiteCastlingRights = bothWaysCastlingRights,
                  blackCastlingRights = bothWaysCastlingRights,
                  pawnCapturableEnPassant = Nothing,
                  pieces = initialPieces
                },
          plies = [],
          pliesWithoutPawnOrCapture = 0,
          numberOfMoves = 1
        }
