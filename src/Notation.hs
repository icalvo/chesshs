module Notation where

import Actions (PlayerAction (..), PlayerActionOutcome (..), Representable (..), moves')
import BoardElements
import Chess
import Data.Function ((&))
import Plies

class AlgebraicNotation a where
  pgn :: a -> String
  pgnCommented :: a -> String
  pgnCommented = pgn

instance AlgebraicNotation Shape where
  pgn p =
    case p of
      Pawn -> "P"
      Knight -> "N"
      Bishop -> "B"
      Rook -> "R"
      Queen -> "Q"
      King -> "K"

instance AlgebraicNotation Rank where
  pgn r = show (fromEnum r + 1)

instance AlgebraicNotation File where
  pgn f =
    case f of
      A -> "a"
      B -> "b"
      C -> "c"
      D -> "d"
      E -> "e"
      F -> "f"
      G -> "g"
      H -> "h"

instance AlgebraicNotation Coordinate where
  pgn (Coordinate (f, r)) = (pgn f) ++ (pgn r)

instance AlgebraicNotation Piece where
  pgn (Piece White shape) = pgn shape
  pgn (Piece Black shape) = pgn shape

instance AlgebraicNotation PlyOutput where
  pgn plyOutput =
    let shapeString s =
          case s of Pawn -> ""; _ -> pgn s
        checkSuffix = if isCheck plyOutput then "+" else ""
        ply' = ply plyOutput
        restOfPlies' = restOfPlies plyOutput
        ambiguousPlies :: [Ply]
        ambiguousPlies =
          restOfPlies'
            & filter
              ( \p ->
                  source p /= source ply'
                    && plyType p == plyType ply'
                    && shape p == shape ply'
                    && target p == target ply'
              )
        generalSourceDiscriminator src@(Coordinate (f, r)) =
          let otherAtSameRank = any (\x -> rank (source x) == r) ambiguousPlies
              otherAtSameFile = any (\x -> file (source x) == f) ambiguousPlies
              rankDiscriminator =
                if null ambiguousPlies || (not (otherAtSameRank && otherAtSameFile))
                  then
                    ""
                  else
                    pgn src
              fileDiscriminator = if otherAtSameFile then pgn src else ""
           in rankDiscriminator ++ fileDiscriminator
        moveSourceDiscriminator shape src =
          case shape of
            Pawn -> ""
            King -> ""
            _ -> generalSourceDiscriminator src

        captureSourceDiscriminator shape src =
          case shape of
            Pawn -> pgn src
            King -> ""
            _ -> generalSourceDiscriminator src

        algebraicWithoutSuffix =
          case ply' of
            Move (Piece _ shape) s t ->
              let sourceDiscriminator = moveSourceDiscriminator shape s
                  targetPosString = pgn t
               in shapeString shape ++ sourceDiscriminator ++ targetPosString
            Capture (Piece _ shape) s t ->
              let sourceDiscriminator = captureSourceDiscriminator shape s
                  targetPosString = pgn t
               in shapeString shape ++ sourceDiscriminator ++ targetPosString
            CaptureEnPassant _ s t ->
              let sourceDiscriminator = captureSourceDiscriminator Pawn s
                  targetPosString = pgn t
               in shapeString Pawn ++ sourceDiscriminator ++ targetPosString
            CastleKingSide {} -> "O-O"
            CastleQueenSide {} -> "O-O-O"
            Promote _ _ t targetShape ->
              pgn t ++ "=" ++ pgn targetShape
            CaptureAndPromote _ s t targetShape ->
              pgn s ++ "x" ++ pgn t ++ "=" ++ pgn targetShape
     in algebraicWithoutSuffix ++ checkSuffix

instance Show PlayerAction where
  show (MovePiece ply restOfPlies drawOffer) =
    pgn (PlyOutput {ply = ply, restOfPlies = restOfPlies, isCheck = False, drawOffer = False})
      ++ (if drawOffer then ":d" else "")
  show Resign = ":r"
  show AcceptDraw = ":a"

data Pair a = SinglePair a | Pair a a

instance AlgebraicNotation PlayerActionOutcome where
  pgn outcome =
    let pairs (x : y : xs) = Pair x y : (pairs xs)
        pairs (x : []) = [SinglePair x]
        pairs [] = []
        indexedFrom :: Int -> [a] -> [(Int, a)]
        indexedFrom i xs = zip [i ..] xs
        movesToPGN moves =
          moves
            & pairs
            & indexedFrom 1
            & map
              ( \(idx, plies) ->
                  case plies of
                    SinglePair whitePly -> show idx ++ ". " ++ pgn whitePly
                    Pair whitePly blackPly -> show idx ++ ". " ++ pgn whitePly ++ " " ++ pgn blackPly
              )
            & concat
        x =
          case outcome of
            Draw {} -> " 1/2-1/2"
            LostByResignation _ player ->
              case player of
                Black -> " 1-0"
                White -> " 0-1"
            WonByCheckmate _ player ->
              case player of
                Black -> "# 1-0"
                White -> "# 0-1"
            _ -> ""
     in movesToPGN (moves' (representation outcome)) ++ x
