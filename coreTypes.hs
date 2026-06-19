{-# LANGUAGE TupleSections #-}

module CoreTypes
  ( Shape (..),
    Rank (..),
    File (..),
    Color (..),
    Piece (..),
    CastlingRights (..),
    bothWaysCastlingRights,
    noCastlingRights,
    Colored (..),
    nextRank,
    prevRank,
    nextFile,
    prevFile,
    Coordinate,
    whitePawn,
    whiteKnight,
    whiteBishop,
    whiteRook,
    whiteQueen,
    whiteKing,
    blackPawn,
    blackKnight,
    blackBishop,
    blackRook,
    blackQueen,
    blackKing,
    a8,
    b8,
    c8,
    d8,
    e8,
    f8,
    g8,
    h8,
    a7,
    b7,
    c7,
    d7,
    e7,
    f7,
    g7,
    h7,
    a6,
    b6,
    c6,
    d6,
    e6,
    f6,
    g6,
    h6,
    a5,
    b5,
    c5,
    d5,
    e5,
    f5,
    g5,
    h5,
    a4,
    b4,
    c4,
    d4,
    e4,
    f4,
    g4,
    h4,
    a3,
    b3,
    c3,
    d3,
    e3,
    f3,
    g3,
    h3,
    a2,
    b2,
    c2,
    d2,
    e2,
    f2,
    g2,
    h2,
    a1,
    b1,
    c1,
    d1,
    e1,
    f1,
    g1,
    h1,
  )
where

data Shape
  = Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King
  deriving (Eq, Show)

data Rank
  = R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  deriving (Ord, Bounded, Eq, Enum)

instance Show Rank where
  show r = show (fromEnum r + 1)

data File = A | B | C | D | E | F | G | H deriving (Enum, Show, Eq, Ord, Bounded)

type Coordinate = (File, Rank)

trySucc :: (Enum a, Eq a, Bounded a) => a -> Maybe a
trySucc x = if x == maxBound then Nothing else Just (succ x)

tryPred :: (Enum a, Eq a, Bounded a) => a -> Maybe a
tryPred x = if x == minBound then Nothing else Just (pred x)

nextRank :: Coordinate -> Maybe Coordinate
nextRank (f, r) = fmap (f,) (trySucc r)

prevRank :: Coordinate -> Maybe Coordinate
prevRank (f, r) = fmap (f,) (tryPred r)

nextFile :: Coordinate -> Maybe Coordinate
nextFile (f, r) = fmap (,r) (trySucc f)

prevFile :: Coordinate -> Maybe Coordinate
prevFile (f, r) = fmap (,r) (tryPred f)

data Color = Black | White deriving (Eq, Show)

class Colored a where
  color :: a -> Color

instance Colored Color where
  color = id

data Piece = Piece Color Shape deriving (Eq, Show)

instance Colored Piece where
  color (Piece c _) = c

whitePawn = Piece White Pawn

whiteKnight = Piece White Knight

whiteBishop = Piece White Bishop

whiteRook = Piece White Rook

whiteQueen = Piece White Queen

whiteKing = Piece White King

blackPawn = Piece Black Pawn

blackKnight = Piece Black Knight

blackBishop = Piece Black Bishop

blackRook = Piece Black Rook

blackQueen = Piece Black Queen

blackKing = Piece Black King

data CastlingRights = CastlingRights
  { canCastleKingSide :: Bool,
    canCastleQueenSide :: Bool
  }
  deriving (Eq, Show)

bothWaysCastlingRights = CastlingRights {canCastleKingSide = True, canCastleQueenSide = True}

noCastlingRights = CastlingRights {canCastleKingSide = False, canCastleQueenSide = False}

a1 = (A, R1)

a2 = (A, R2)

a3 = (A, R3)

a4 = (A, R4)

a5 = (A, R5)

a6 = (A, R6)

a7 = (A, R7)

a8 = (A, R8)

b1 = (B, R1)

b2 = (B, R2)

b3 = (B, R3)

b4 = (B, R4)

b5 = (B, R5)

b6 = (B, R6)

b7 = (B, R7)

b8 = (B, R8)

c1 = (C, R1)

c2 = (C, R2)

c3 = (C, R3)

c4 = (C, R4)

c5 = (C, R5)

c6 = (C, R6)

c7 = (C, R7)

c8 = (C, R8)

d1 = (D, R1)

d2 = (D, R2)

d3 = (D, R3)

d4 = (D, R4)

d5 = (D, R5)

d6 = (D, R6)

d7 = (D, R7)

d8 = (D, R8)

e1 = (E, R1)

e2 = (E, R2)

e3 = (E, R3)

e4 = (E, R4)

e5 = (E, R5)

e6 = (E, R6)

e7 = (E, R7)

e8 = (E, R8)

f1 = (F, R1)

f2 = (F, R2)

f3 = (F, R3)

f4 = (F, R4)

f5 = (F, R5)

f6 = (F, R6)

f7 = (F, R7)

f8 = (F, R8)

g1 = (G, R1)

g2 = (G, R2)

g3 = (G, R3)

g4 = (G, R4)

g5 = (G, R5)

g6 = (G, R6)

g7 = (G, R7)

g8 = (G, R8)

h1 = (H, R1)

h2 = (H, R2)

h3 = (H, R3)

h4 = (H, R4)

h5 = (H, R5)

h6 = (H, R6)

h7 = (H, R7)

h8 = (H, R8)
