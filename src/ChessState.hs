{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module ChessState
  ( CastlingRights (..),
    bothWaysCastlingRights,
    RepetitionStateClass (..),
    ChessStateClass (..),
    ChessState,
    Square,
    (@@@),
    isPlayerInCheck,
    initialGameState,
    pieceCapabilities,
    nextGameState,
    opponent,
    repetitionCount,
  )
where

import BoardElements
import Chess
import Data.Function ((&))
import Data.Kind
import Data.List
import Data.List.NonEmpty (NonEmpty, (<|))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, mapMaybe, maybeToList)
import Names
import Plies
import Reach

data CastlingRights = CastlingRights
  { canCastleKingSide :: Bool,
    canCastleQueenSide :: Bool
  }
  deriving (Eq, Show)

bothWaysCastlingRights = CastlingRights {canCastleKingSide = True, canCastleQueenSide = True}

class RepetitionStateClass r where
  turn :: r -> Color
  whiteCastlingRights :: r -> CastlingRights
  blackCastlingRights :: r -> CastlingRights
  pawnCapturableEnPassant :: r -> Maybe Coordinate
  pieces :: r -> Map.Map Coordinate Piece

data RepetitionState = RepetitionState
  { turn' :: Color,
    whiteCastlingRights' :: CastlingRights,
    blackCastlingRights' :: CastlingRights,
    pawnCapturableEnPassant' :: Maybe Coordinate,
    pieces' :: Map.Map Coordinate Piece
  }
  deriving (Eq)

instance RepetitionStateClass RepetitionState where
  turn (RepetitionState t _ _ _ _) = t
  whiteCastlingRights (RepetitionState _ wcr _ _ _) = wcr
  blackCastlingRights (RepetitionState _ _ bcr _ _) = bcr
  pawnCapturableEnPassant (RepetitionState _ _ _ pcep _) = pcep
  pieces (RepetitionState _ _ _ _ pcs) = pcs

class (RepetitionStateClass cs, RepetitionStateClass (Elem cs)) => ChessStateClass cs where
  type Elem cs :: Type
  plies :: cs -> [PlyOutput]
  pliesWithoutPawnOrCapture :: cs -> Int
  repeatableStates :: cs -> NonEmpty (Elem cs)
  numberOfMoves :: cs -> Int

-- In chess, the state of the game is defined by several properties in addition
-- to the pieces on the board. We need to know the castling rights of each player,
-- whether the last move allows en passant capture (and the coordinate of the pawn that
-- can be captured), the repeated coordinates and their number, and the number of
-- moves since a pawn moved or a capture happened.
data ChessState = ChessState
  { plies' :: [PlyOutput],
    pliesWithoutPawnOrCapture' :: Int,
    repeatableStates' :: NonEmpty RepetitionState,
    numberOfMoves' :: Int
  }
  deriving (Eq)

repState :: ChessState -> RepetitionState
repState = repeatableStates' -. NE.head

instance ChessStateClass ChessState where
  type Elem ChessState = RepetitionState
  plies = plies'
  pliesWithoutPawnOrCapture = pliesWithoutPawnOrCapture'
  repeatableStates = repeatableStates'
  numberOfMoves = numberOfMoves

instance RepetitionStateClass ChessState where
  turn = repState -. turn'
  whiteCastlingRights = repState -. whiteCastlingRights'
  blackCastlingRights = repState -. blackCastlingRights'
  pawnCapturableEnPassant = repState -. pawnCapturableEnPassant'
  pieces = repState -. pieces'

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
        { repeatableStates' =
            NE.singleton
              RepetitionState
                { turn' = White,
                  whiteCastlingRights' = bothWaysCastlingRights,
                  blackCastlingRights' = bothWaysCastlingRights,
                  pawnCapturableEnPassant' = Nothing,
                  pieces' = initialPieces
                },
          plies' = [],
          pliesWithoutPawnOrCapture' = 0,
          numberOfMoves' = 1
        }

repetitionCount game =
  repeatableStates game
    & NE.filter (repState game ==)
    & length

(@@@) :: (ChessStateClass c) => c -> Coordinate -> Square
game @@@ coord = (coord, Map.lookup coord (pieces game))

-- Generates plies from a ply type, a piece, and a source and target coordinates.
-- In general, a single ply is generated. However, when there is a promotion, there
-- are 4 possible plies, one for each kind of promoted piece (=Q, =R, =B & =N).
toPlies src (p@(Piece color sh), plyType, tgt) =
  case plyType of
    MoveType -> [Move p src tgt]
    CaptureType -> [Capture p src tgt]
    EnPassantType -> [CaptureEnPassant color src tgt]
    CastleKingSideType -> [CastleKingSide color]
    CastleQueenSideType -> [CastleQueenSide color]
    MoveAndPromoteType -> [Queen, Rook, Bishop, Knight] & map (Promote color src tgt)
    CaptureAndPromoteType -> [Queen, Rook, Bishop, Knight] & map (CaptureAndPromote color src tgt)

-- Can the ply type be legally executed for a piece with a given target coordinate?
canExecute game sourcePiece targetCoord plyType =
  let targetSquare = game @@@ targetCoord
   in case plyType of
        MoveType -> targetSquare & isEmpty
        MoveAndPromoteType -> targetSquare & isEmpty
        CaptureType ->
          (targetSquare & hasPiece)
            && (targetSquare & differentColor sourcePiece)
        CaptureAndPromoteType ->
          (targetSquare & hasPiece)
            && (targetSquare & differentColor sourcePiece)
        EnPassantType ->
          case pawnCapturableEnPassant game of
            Just capturable -> capturable == targetCoord
            Nothing -> False
        CastleKingSideType ->
          case sourcePiece of
            Piece White King ->
              (game & whiteCastlingRights & canCastleKingSide)
                && (game @@@ f1 & isEmpty)
                && (game @@@ g1 & isEmpty)
            Piece Black King ->
              (game & blackCastlingRights & canCastleKingSide)
                && (game @@@ f8 & isEmpty)
                && (game @@@ g8 & isEmpty)
            _ -> False
        CastleQueenSideType ->
          case sourcePiece of
            Piece White King ->
              (game & whiteCastlingRights & canCastleQueenSide)
                && (game @@@ b1 & isEmpty)
                && (game @@@ c1 & isEmpty)
                && (game @@@ d1 & isEmpty)
            Piece Black King ->
              (game & blackCastlingRights & canCastleQueenSide)
                && (game @@@ b8 & isEmpty)
                && (game @@@ c8 & isEmpty)
                && (game @@@ d8 & isEmpty)
            _ -> False

evaluateReachSquare :: (ChessStateClass c) => c -> PlyType -> Piece -> Coordinate -> Maybe (Piece, PlyType, Coordinate)
evaluateReachSquare game plyTypeToTest piece targetCoord =
  let canDoPlyType = canExecute game piece targetCoord plyTypeToTest
      canMove = canExecute game piece targetCoord MoveType
   in if canDoPlyType
        then Just (piece, plyTypeToTest, targetCoord)
        -- Even if the ply type cannot be executed, if you can move to the
        -- target coordinate we still return something, so that the ply type
        -- is evaluated further in the same reach.
        else
          if canMove
            then Just (piece, MoveType, targetCoord)
            else Nothing

takeWhileIncludingLast :: (a -> Bool) -> [a] -> [a]
takeWhileIncludingLast p l = span p l & (\(l1, l2) -> l1 ++ (l2 & listToMaybe & maybeToList))

reachCapabilities :: (ChessStateClass c) => c -> Piece -> PlyType -> [Coordinate] -> [(Piece, PlyType, Coordinate)]
reachCapabilities game piece plyTypeToAnalyze reach =
  let actionIs plyTypeToCheck (_, plyType, _) = plyType == plyTypeToCheck
   in reach
        & mapMaybe (evaluateReachSquare game plyTypeToAnalyze piece)
        & takeWhileIncludingLast (actionIs MoveType)
        & filter (actionIs plyTypeToAnalyze)

-- Legal plies for a piece (without taking into account the
-- out-of-check rule).
pieceCapabilitiesWithoutCheckFilter game piece sourceCoord =
  pieceReaches piece sourceCoord
    & concatMap (\(reach, plyType) -> reachCapabilities game piece plyType reach)
    & concatMap (toPlies sourceCoord)

-- Coordinates attacked by a piece.
attacksBy game (coord, piece) =
  pieceCapabilitiesWithoutCheckFilter game piece coord
    & mapMaybe coordOfCapturedPiece

-- Coordinates attacked by a player.
attacks playerColor game =
  pieces game
    & Map.toAscList
    & filter (\(_, Piece pieceColor _) -> pieceColor == playerColor)
    & concatMap (attacksBy game)

-- Is the coordinate attacked by a player?
isAttackedBy playerColor game targetCoord =
  game
    & attacks playerColor
    & elem targetCoord

-- Executes a board change (unchecked).
rawBoardChange :: BoardChange -> Map.Map Coordinate Piece -> Map.Map Coordinate Piece
rawBoardChange boardChange pieces =
  case boardChange of
    MoveBoardPiece sourceCoord targetCoord ->
      case Map.lookup sourceCoord pieces of
        Nothing ->
          pieces
        Just piece ->
          pieces
            & Map.delete sourceCoord
            & Map.delete targetCoord
            & Map.insert targetCoord piece
    RemoveBoardPiece coord ->
      -- assert
      --   (pieces |> Map.member coord)
      pieces
        & Map.delete coord
    AddBoardPiece col shp coord ->
      pieces
        & Map.delete coord
        & Map.insert coord (Piece col shp)

opponent color =
  case color of
    White -> Black
    Black -> White

-- Is the player in check?
isPlayerInCheck playerColor game =
  let possibleKingCoord =
        game
          & pieces
          & Map.toAscList
          & find (\(_, piece) -> piece == Piece playerColor King)
          & fmap fst
   in case possibleKingCoord of
        Just kingCoord -> isAttackedBy (opponent playerColor) game kingCoord
        Nothing -> False

rawExecutePly :: Ply -> Map.Map Coordinate Piece -> Map.Map Coordinate Piece
rawExecutePly ply pieces =
  ply
    & boardChanges
    & foldr rawBoardChange pieces

nextGameState ply restOfPlies drawOffer gameState =
  let plyRevokesWhiteCastleKingSideRights =
        case ply of
          CastleKingSide White -> True
          CastleQueenSide White -> True
          Move (Piece White King) _ _ -> True
          Move (Piece White Rook) (Coordinate (H, R1)) _ -> True
          Capture (Piece White Rook) _ _ -> True
          Capture (Piece White King) _ _ -> True
          Capture _ _ (Coordinate (H, R1)) -> True
          _ -> False
      plyRevokesBlackCastleKingSideRights =
        case ply of
          CastleKingSide Black -> True
          CastleQueenSide Black -> True
          Move (Piece Black King) _ _ -> True
          Move (Piece Black Rook) (Coordinate (H, R8)) _ -> True
          Capture (Piece Black Rook) _ _ -> True
          Capture (Piece Black King) _ _ -> True
          Capture _ _ (Coordinate (H, R8)) -> True
          _ -> False
      plyRevokesWhiteCastleQueenSideRights =
        False
      -- match ply with
      -- \| CastleKingSide White
      -- \| CastleQueenSide White
      -- \| Move    (Piece (White, King), _, _)
      -- \| Move    (Piece (White, Rook), (A, R1), _)
      -- \| Capture (Piece (White, Rook), _, _)
      -- \| Capture (Piece (White, King), _, _)
      -- \| Capture (_, _, (A, R1)) -> true
      -- \| _ -> false
      plyRevokesBlackCastleQueenSideRights =
        False
      -- match ply with
      -- \| CastleKingSide Black
      -- \| CastleQueenSide Black
      -- \| Move    (Piece (Black, King), _, _)
      -- \| Move    (Piece (Black, Rook), (A, R8), _)
      -- \| Capture (Piece (Black, Rook), _, _)
      -- \| Capture (Piece (Black, King), _, _)
      -- \| Capture (_, _, (A, R8)) -> true
      -- \| _ -> false

      hasStructureChanged = isCapture ply || shape ply == Pawn
      repState =
        RepetitionState
          { turn' = opponent (turn gameState),
            pieces' = pieces gameState & rawExecutePly ply,
            pawnCapturableEnPassant' =
              case ply of
                Move whitePawn (Coordinate (_, R2)) (Coordinate (f, R4)) -> Just (Coordinate (f, R3))
                Move blackPawn (Coordinate (_, R7)) (Coordinate (f, R5)) -> Just (Coordinate (f, R6))
                _ -> Nothing,
            blackCastlingRights' =
              CastlingRights
                { canCastleKingSide =
                    (gameState & blackCastlingRights & canCastleKingSide)
                      && not plyRevokesBlackCastleKingSideRights,
                  canCastleQueenSide =
                    (gameState & blackCastlingRights & canCastleQueenSide)
                      && not plyRevokesBlackCastleQueenSideRights
                },
            whiteCastlingRights' =
              CastlingRights
                { canCastleKingSide =
                    (gameState & whiteCastlingRights & canCastleKingSide)
                      && not plyRevokesWhiteCastleKingSideRights,
                  canCastleQueenSide =
                    (gameState & whiteCastlingRights & canCastleQueenSide)
                      && not plyRevokesWhiteCastleQueenSideRights
                }
          }
      nextGameStateTemp =
        gameState
          { pliesWithoutPawnOrCapture' = if hasStructureChanged then 0 else (gameState & pliesWithoutPawnOrCapture) + 1,
            repeatableStates' =
              if hasStructureChanged
                then
                  NE.singleton repState
                else
                  repState <| repeatableStates gameState,
            numberOfMoves' =
              case turn gameState of
                White -> numberOfMoves gameState
                Black -> numberOfMoves gameState + 1
          }

      isInCheck = isPlayerInCheck (turn nextGameStateTemp) nextGameStateTemp

      newPlyOutput =
        PlyOutput
          { ply = ply,
            restOfPlies = restOfPlies,
            isCheck = isInCheck,
            drawOffer = drawOffer
          }
   in nextGameStateTemp
        { plies' = newPlyOutput : plies nextGameStateTemp
        }

pieceCapabilities game (piece, sourceCoord) =
  if isEmpty (game @@@ sourceCoord)
    then
      []
    else
      let plyPutsPlayerInCheck ply =
            nextGameState ply [] False game
              & isPlayerInCheck (turn game)
       in pieceCapabilitiesWithoutCheckFilter game piece sourceCoord
            & filter (not . plyPutsPlayerInCheck)
