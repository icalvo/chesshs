{-# LANGUAGE DisambiguateRecordFields #-}

module Chess where

import ChessRepresentation
import ChessState
import CoreTypes
import Data.Function ((&))
import Data.List
import Data.List.NonEmpty (NonEmpty, (<|))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Plies
import Reach

a -. b = b . a

placedPiece color shape pos = (pos, Piece color shape)

opposite color =
  case color of
    White -> Black
    Black -> White

type Square = (Coordinate, Maybe Piece)

isEmpty = snd -. isNothing

hasPiece = snd -. isJust

pieceColor = snd -. fmap color

piece = snd

pieceCoord = fst

squareAt :: Coordinate -> Map.Map Coordinate Piece -> Square
squareAt coord placedPieces = (coord, Map.lookup coord placedPieces)

placedPieces @@ coord = squareAt coord placedPieces

data BoardChange
  = MovePiece Coordinate Coordinate
  | RemovePiece Coordinate
  | AddPiece Color Shape Coordinate
  deriving (Eq, Show)

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
      [ MovePiece source target
      ]
    Capture _ source target ->
      [ RemovePiece target,
        MovePiece source target
      ]
    CaptureEnPassant _ source@(_, sr) target@(tf, _) ->
      [ let coordOfCapturedPawn = (tf, sr)
         in RemovePiece coordOfCapturedPawn,
        MovePiece source target
      ]
    CastleKingSide White ->
      [ MovePiece e1 g1,
        MovePiece h1 f1
      ]
    CastleQueenSide White ->
      [ MovePiece e1 c1,
        MovePiece a1 d1
      ]
    CastleKingSide Black ->
      [ MovePiece e8 g8,
        MovePiece h8 f8
      ]
    CastleQueenSide Black ->
      [ MovePiece e8 c8,
        MovePiece a8 d8
      ]
    Promote color source target shape ->
      [ RemovePiece source,
        AddPiece color shape target
      ]
    CaptureAndPromote color source target shape ->
      [ RemovePiece source,
        AddPiece color shape target
      ]

coordOfCapturedPiece ply =
  case ply of
    Capture _ _ target -> Just target
    CaptureAndPromote _ _ target _ -> Just target
    CaptureEnPassant _ (_, sourceRank) (targetFile, _) -> Just (targetFile, sourceRank)
    _ -> Nothing

sameColor (Piece sourceColor _) square =
  case pieceColor square of
    Just col -> col == sourceColor
    Nothing -> False

differentColor p s = not (sameColor p s)

currentPlayerCastlingRights this =
  case turn this of
    White -> whiteCastlingRights this
    Black -> blackCastlingRights this

repetitionCount game =
  repeatableStates game
    & NE.filter ((repState game) ==)
    & length

game @@@ coord = pieces game @@ coord

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

evaluateReachSquare :: ChessState -> PlyType -> Piece -> Coordinate -> Maybe (Piece, PlyType, Coordinate)
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

reachCapabilities :: ChessState -> Piece -> PlyType -> [Coordinate] -> [(Piece, PlyType, Coordinate)]
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
    MovePiece sourceCoord targetCoord ->
      case Map.lookup sourceCoord pieces of
        Nothing ->
          pieces
        Just piece ->
          pieces
            & Map.delete sourceCoord
            & Map.delete targetCoord
            & Map.insert targetCoord piece
    RemovePiece coord ->
      -- assert
      --   (pieces |> Map.member coord)
      pieces
        & Map.delete coord
    AddPiece col shp coord ->
      pieces
        & Map.delete coord
        & Map.insert coord (Piece col shp)

opponent = opposite

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
          Move (Piece White Rook) (H, R1) _ -> True
          Capture (Piece White Rook) _ _ -> True
          Capture (Piece White King) _ _ -> True
          Capture _ _ (H, R1) -> True
          _ -> False
      plyRevokesBlackCastleKingSideRights =
        case ply of
          CastleKingSide Black -> True
          CastleQueenSide Black -> True
          Move (Piece Black King) _ _ -> True
          Move (Piece Black Rook) (H, R8) _ -> True
          Capture (Piece Black Rook) _ _ -> True
          Capture (Piece Black King) _ _ -> True
          Capture _ _ (H, R8) -> True
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
          { turn = opponent (turn gameState),
            pieces = pieces gameState & rawExecutePly ply,
            pawnCapturableEnPassant =
              case ply of
                Move whitePawn (_, R2) (f, R4) -> Just (f, R3)
                Move blackPawn (_, R7) (f, R5) -> Just (f, R6)
                _ -> Nothing,
            blackCastlingRights =
              CastlingRights
                { canCastleKingSide =
                    (gameState & blackCastlingRights & canCastleKingSide)
                      && not plyRevokesBlackCastleKingSideRights,
                  canCastleQueenSide =
                    (gameState & blackCastlingRights & canCastleQueenSide)
                      && not plyRevokesBlackCastleQueenSideRights
                },
            whiteCastlingRights =
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
          { pliesWithoutPawnOrCapture = if hasStructureChanged then 0 else (gameState & pliesWithoutPawnOrCapture) + 1,
            repeatableStates =
              if hasStructureChanged
                then
                  NE.singleton repState
                else
                  repState <| repeatableStates gameState,
            numberOfMoves =
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
        { plies = newPlyOutput : plies nextGameStateTemp
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
