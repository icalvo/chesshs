{-# LANGUAGE TupleSections #-}

module Reach
  ( Reach (..),
    pieceReaches,
  )
where

import BoardElements
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe (mapMaybe)
import Plies

type Reach = [[Coordinate]]

-- Directions
up = nextRank

upRight = nextFile >=> nextRank

right = nextFile

downRight = nextFile >=> prevRank

down = prevRank

downLeft = prevFile >=> prevRank

left = prevFile

upLeft = prevFile >=> nextRank

unfoldSimple :: (a -> Maybe a) -> a -> [a]
unfoldSimple f =
  let dupe a = (a, a)
      tt x = fmap dupe (f x)
   in unfoldr tt

bishopReaches :: Coordinate -> Reach
bishopReaches pos =
  [upRight, downRight, downLeft, upLeft]
    & map (($ pos) . unfoldSimple)
    & filter (not . null)

rookReaches pos =
  [up, right, down, left]
    & map (($ pos) . unfoldSimple)
    & filter (not . null)

queenReaches pos =
  map (concat . ($ pos)) [bishopReaches, rookReaches]

kingReaches pos =
  [upRight, downRight, downLeft, upLeft, up, right, down, left]
    & mapMaybe ($ pos)
    & map singleton

knightReaches pos =
  [ up >=> up >=> left,
    up >=> up >=> right,
    down >=> down >=> left,
    down >=> down >=> right,
    right >=> right >=> up,
    right >=> right >=> down,
    left >=> left >=> up,
    left >=> left >=> down
  ]
    & mapMaybe ($ pos)
    & map singleton

pawnDir :: Color -> (Coordinate -> Maybe Coordinate)
pawnDir White = up
pawnDir Black = down

pawnSingleMoveReaches :: Color -> Coordinate -> Reach
pawnSingleMoveReaches pawnColor pos =
  case pawnDir pawnColor pos of
    Just newPos -> singleton (singleton newPos)
    Nothing -> []

pawnDoubleMoveReaches :: Color -> Coordinate -> Reach
pawnDoubleMoveReaches pawnColor pos =
  unfoldSimple (pawnDir pawnColor) pos
    & take 2
    & singleton

pawnCaptureReaches pawnColor pos =
  let dirFunc = pawnDir pawnColor
   in [dirFunc >=> left, dirFunc >=> right]
        & mapMaybe ($ pos)
        & map singleton

(>*>) :: (Monad m) => (a -> m a) -> Int -> (a -> m a)
(>*>) f n = iterate (>=> f) f !! n

data Castling = KingSide | QueenSide

castlingDir :: Castling -> (Coordinate -> Maybe Coordinate)
castlingDir KingSide = right
castlingDir QueenSide = left

castlingDist :: Castling -> Int
castlingDist KingSide = 2
castlingDist QueenSide = 3

kingCastleReach castling pos =
  [castlingDir castling >*> castlingDist castling]
    & mapMaybe ($ pos)
    & map singleton

data PawnRankType
  = PromotionRank
  | EnPassantRank
  | DoubleMoveRank
  | RegularRank

pieceReaches piece pos =
  let applyReaches :: (Coordinate -> Reach, PlyType) -> [([Coordinate], PlyType)]
      applyReaches (reachesfn, act) =
        pos
          & reachesfn
          & filter (not . null)
          & map (,act)

      collectReaches = concatMap applyReaches
   in case piece of
        Piece color Pawn ->
          let Coordinate (_, rank) = pos

              pawnRankType =
                case (color, rank) of
                  (White, R7) -> PromotionRank
                  (Black, R2) -> PromotionRank
                  (White, R5) -> EnPassantRank
                  (Black, R4) -> EnPassantRank
                  (White, R2) -> DoubleMoveRank
                  (Black, R7) -> DoubleMoveRank
                  _ -> RegularRank
           in case pawnRankType of
                PromotionRank ->
                  collectReaches
                    [ (pawnSingleMoveReaches color, MoveAndPromoteType),
                      (pawnCaptureReaches color, CaptureAndPromoteType)
                    ]
                EnPassantRank ->
                  collectReaches
                    [ (pawnSingleMoveReaches color, MoveType),
                      (pawnCaptureReaches color, CaptureType),
                      (pawnCaptureReaches color, EnPassantType)
                    ]
                DoubleMoveRank ->
                  collectReaches
                    [ (pawnDoubleMoveReaches color, MoveType),
                      (pawnCaptureReaches color, CaptureType)
                    ]
                RegularRank ->
                  collectReaches
                    [ (pawnSingleMoveReaches color, MoveType),
                      (pawnCaptureReaches color, CaptureType)
                    ]
        Piece _ Knight ->
          collectReaches
            [ (knightReaches, MoveType),
              (knightReaches, CaptureType)
            ]
        Piece _ Bishop ->
          collectReaches
            [ (bishopReaches, MoveType),
              (bishopReaches, CaptureType)
            ]
        Piece _ Rook ->
          collectReaches
            [ (rookReaches, MoveType),
              (rookReaches, CaptureType)
            ]
        Piece _ Queen ->
          collectReaches
            [ (queenReaches, MoveType),
              (queenReaches, CaptureType)
            ]
        Piece White King ->
          collectReaches
            [ (kingReaches, MoveType),
              (kingReaches, CaptureType),
              (kingCastleReach KingSide, CastleKingSideType),
              (kingCastleReach QueenSide, CastleQueenSideType)
            ]
        Piece Black King ->
          collectReaches
            [ (kingReaches, MoveType),
              (kingReaches, CaptureType),
              (kingCastleReach KingSide, CastleKingSideType),
              (kingCastleReach QueenSide, CastleQueenSideType)
            ]
