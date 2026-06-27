{- HLINT ignore "Redundant bracket" -}
module Parsing where

import BoardElements
import ChessState (CastlingRights (CastlingRights, canCastleKingSide, canCastleQueenSide), ChessState, buildGameState)
import Data.Char (digitToInt)
import Data.Function ((&))
import Data.Functor (($>))
import Data.List (singleton)
import Data.Map (Map, fromList)
import Data.Maybe (catMaybes)
import Names
import Text.Parsec

type Parser = Parsec String ()

ppiece :: Parser Piece
ppiece =
  choice
    [ whiteKing <$ char 'K',
      whiteQueen <$ char 'Q',
      whiteRook <$ char 'R',
      whiteBishop <$ char 'B',
      whiteKnight <$ char 'N',
      whitePawn <$ char 'P',
      blackKing <$ char 'k',
      blackQueen <$ char 'q',
      blackRook <$ char 'r',
      blackBishop <$ char 'b',
      blackKnight <$ char 'n',
      blackPawn <$ char 'p'
    ]

pplayer :: Parser Color
pplayer =
  choice
    [ White <$ char 'w',
      Black <$ char 'b'
    ]

pc :: Char -> Parser Bool
pc c = option False (char c $> True)

bcr k q = CastlingRights {canCastleKingSide = k, canCastleQueenSide = q}

pcrw :: Parser CastlingRights
pcrw = bcr <$> pc 'K' <*> pc 'Q'

pcrb :: Parser CastlingRights
pcrb = bcr <$> pc 'k' <*> pc 'q'

pemptySquareNumber :: Parser [Maybe Piece]
pemptySquareNumber = (\rep -> replicate (digitToInt rep) Nothing) <$> oneOf "12345678"

psquare = (singleton . Just <$> ppiece) <|> pemptySquareNumber

prow :: Parser [Maybe Piece]
prow = concat <$> many psquare

toSquare r f piece = (Coordinate (toEnum f :: File, toEnum r :: Rank), piece)

indexedFrom :: Int -> [a] -> [(Int, a)]
indexedFrom i = zip [i ..]

mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f l = map (uncurry f) (indexedFrom 0 l)

toMap :: [[Maybe Piece]] -> Either String (Map Coordinate Piece)
toMap ll =
  let lengthIsNot8 = (/=) 8 . length
   in if lengthIsNot8 ll
        then
          Left "There are %i{List.length ll} rows and there should be 8"
        else
          let badRows = filter lengthIsNot8 ll
           in -- index = 1 + findIndex (not . lengthIs8) ll

              if not (null badRows)
                then
                  Left "There are some rows that have not 8 columns; the first one is #%i{index} with %i{List.item index ll |> List.length} columns"
                else
                  ll
                    & mapi (\r -> mapi (fmap . toSquare r))
                    & concat
                    & catMaybes
                    & fromList
                    & Right

pboard :: Parser (Map Coordinate Piece)
pboard = sepBy prow (char '/') >>= either fail return . toMap

pfile :: Parser File
pfile =
  choice
    [ char 'a' $> A,
      char 'b' $> B,
      char 'c' $> C,
      char 'd' $> D,
      char 'e' $> E,
      char 'f' $> F,
      char 'g' $> G,
      char 'h' $> H
    ]

prank :: Parser Rank
prank = ((\i -> toEnum (i - 1) :: Rank) . digitToInt) <$> (oneOf "12345678")

pcoord = (curry Coordinate) <$> pfile <*> prank

penpassant =
  choice
    [ char '-' $> Nothing,
      Just <$> pcoord
    ]

integer :: Parser Int
integer = read <$> many1 digit

pstate :: Parser ChessState
pstate =
  buildGameState
    <$> pboard
    <*> (pplayer <* space)
    <*> pcrw
    <*> (pcrb <* space)
    <*> (penpassant <* space)
    <*> (integer <* space)
    <*> integer
