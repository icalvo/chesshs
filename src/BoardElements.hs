{-# LANGUAGE TupleSections #-}

module BoardElements
  ( (-.),
    Shape (..),
    Rank (..),
    File (..),
    Coordinate (..),
    rank,
    file,
    Piece (..),
    Color (..),
    Colored (..),
    pieceColor,
    nextRank,
    prevRank,
    nextFile,
    prevFile,
    Square,
    isEmpty,
    hasPiece,
  )
where

import Data.Maybe (isJust, isNothing)

a -. b = b . a

data Shape
  = Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King
  deriving (Eq)

data Rank
  = R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  deriving (Ord, Bounded, Eq, Enum, Show)

data File = A | B | C | D | E | F | G | H deriving (Enum, Eq, Ord, Bounded, Show)

newtype Coordinate = Coordinate {coord :: (File, Rank)} deriving (Eq, Ord, Show)

rank (Coordinate (_, r)) = r

file (Coordinate (f, _)) = f

trySucc :: (Enum a, Eq a, Bounded a) => a -> Maybe a
trySucc x = if x == maxBound then Nothing else Just (succ x)

tryPred :: (Enum a, Eq a, Bounded a) => a -> Maybe a
tryPred x = if x == minBound then Nothing else Just (pred x)

nextRank :: Coordinate -> Maybe Coordinate
nextRank (Coordinate (f, r)) = fmap (Coordinate . (f,)) (trySucc r)

prevRank :: Coordinate -> Maybe Coordinate
prevRank (Coordinate (f, r)) = fmap (Coordinate . (f,)) (tryPred r)

nextFile :: Coordinate -> Maybe Coordinate
nextFile (Coordinate (f, r)) = fmap (Coordinate . (,r)) (trySucc f)

prevFile :: Coordinate -> Maybe Coordinate
prevFile (Coordinate (f, r)) = fmap (Coordinate . (,r)) (tryPred f)

data Color = Black | White deriving (Eq)

instance Show Color where
  show Black = "b"
  show White = "w"

class Colored a where
  color :: a -> Color

instance Colored Color where
  color = id

data Piece = Piece Color Shape deriving (Eq)

instance Colored Piece where
  color (Piece c _) = c

type Square = (Coordinate, Maybe Piece)

pieceColor :: (Colored b) => (a, Maybe b) -> Maybe Color
pieceColor = snd -. fmap color

isEmpty = snd -. isNothing

hasPiece = snd -. isJust
