module Chess where

import Control.Monad
import CoreTypes
import Data.Function ((&))
import Data.List
import Data.Maybe (mapMaybe)

data Piece = Piece Color Shape deriving (Eq, Show)

placedPiece color shape pos = (pos, Piece color shape)

data PlyType
  = MoveType
  | CaptureType
  | EnPassantType
  | CastleKingSideType
  | CastleQueenSideType
  | MoveAndPromoteType
  | CaptureAndPromoteType
  deriving (Eq, Show)

type Reach = [[(File, Rank)]]

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

bishopReaches :: Position -> Reach
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

data PawnDirection = Up | Down

pawnDir :: PawnDirection -> (Position -> Maybe Position)
pawnDir Up = up
pawnDir Down = down

pawnSingleMoveReaches :: PawnDirection -> Position -> Reach
pawnSingleMoveReaches pawnDirection pos =
  case pawnDir pawnDirection pos of
    Just newPos -> singleton (singleton newPos)
    Nothing -> []

pawnDoubleMoveReaches :: PawnDirection -> Position -> Reach
pawnDoubleMoveReaches pawnDirection pos =
  unfoldSimple (pawnDir pawnDirection) pos
    & take 2
    & singleton

pawnCaptureReaches pawnDirection pos =
  let dirFunc = pawnDir pawnDirection
   in [dirFunc >=> left, dirFunc >=> right]
        & mapMaybe ($ pos)
        & map singleton

(>*>) :: (Monad m) => (a -> m a) -> Int -> (a -> m a)
(>*>) f n = iterate (>=> f) f !! n

data CastlingDirection = CRight | CLeft

castlingDir :: CastlingDirection -> (Position -> Maybe Position)
castlingDir CRight = right
castlingDir CLeft = left

kingCastleToKingReach castlingDirection pos =
  [castlingDir castlingDirection >*> 2]
    & mapMaybe ($ pos)
    & map singleton

kingCastleToQueenReach castlingDirection pos =
  [castlingDir castlingDirection >*> 3]
    & mapMaybe ($ pos)
    & map singleton


data PawnRankType =
      PromotionRank
    | EnPassantRank
    | DoubleMoveRank
    | RegularRank

pieceReaches piece pos =
    let
        applyReaches :: (Position -> Reach, PlyType) -> [([Position], PlyType)]
        applyReaches (reachesfn, act) =
            pos
            & reachesfn
            & filter (not . null)
            & map (\reach -> (reach, act))

        collectReaches = concat applyReaches
    in

        case piece of 
        Piece color Pawn ->
            let
              (_, rank) = pos
              direction = case color of {White -> Up; Black -> Down}

              pawnRankType =
                case (color, rank) of
                (White, R7) -> PromotionRank
                (Black, R2) -> PromotionRank
                (White, R5) -> EnPassantRank
                (Black, R4) -> EnPassantRank
                (White, R2) -> DoubleMoveRank
                (Black, R7) -> DoubleMoveRank
                _ -> RegularRank
            in
            case pawnRankType of
            PromotionRank ->
                collectReaches [
                    (pawnSingleMoveReaches direction, MoveAndPromoteType),
                    (pawnCaptureReaches    direction, CaptureAndPromoteType)]
            EnPassantRank ->
                collectReaches [
                    (pawnSingleMoveReaches direction, MoveType),
                    (pawnCaptureReaches    direction, CaptureType),
                    (pawnCaptureReaches    direction, EnPassantType)]
            DoubleMoveRank ->
                collectReaches [
                    (pawnDoubleMoveReaches direction, MoveType   ),
                    (pawnCaptureReaches    direction, CaptureType)
                ]
            RegularRank ->
                collectReaches [
                    (pawnSingleMoveReaches direction, MoveType   ),
                    (pawnCaptureReaches    direction, CaptureType)
                ]
        Piece _ Knight ->
            collectReaches [
                (knightReaches, MoveType),
                (knightReaches, CaptureType)
            ]
        Piece _ Bishop ->
            collectReaches [
                (bishopReaches, MoveType),
                (bishopReaches, CaptureType)
            ]
        Piece _ Rook ->
            collectReaches [
                (rookReaches, MoveType),
                (rookReaches, CaptureType)
            ]
        Piece _ Queen ->
            collectReaches [
                (queenReaches, MoveType),
                (queenReaches, CaptureType)
             ]
        Piece White King ->
            collectReaches [
                (kingReaches, MoveType),
                (kingReaches, CaptureType),
                (kingCastleToKingReach  Right, CastleKingSideType),
                (kingCastleToQueenReach Left , CastleQueenSideType)
            ]
        Piece Black King ->
            collectReaches [
                (kingReaches, MoveType),
                (kingReaches, CaptureType),
                (kingCastleToKingReach  Left , CastleKingSideType),
                (kingCastleToQueenReach Right, CastleQueenSideType)
            ]

