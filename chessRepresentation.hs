module ChessRepresentation
  (
  )
where

import Chess
import CoreTypes
import Data.Array

data ChessStateRepresentation = ChessStateRepresentation
  { board :: Array (Int, Int) Square,
    playerInTurn :: Player,
    isCheck :: Bool,
    whitePlayerCastlingRights :: CastlingRights,
    blackPlayerCastlingRights :: CastlingRights,
    pawnCapturableEnPassant :: Maybe Coordinate,
    pliesWithoutPawnOrCapture :: Int,
    moves :: [PlyOutput],
    numberOfMoves :: Int
  }

data DrawType
  = Agreement
  | Stalemate
  | FiftyMovements
  | ThreefoldRepetition
  | FivefoldRepetition
  | SeventyFiveMovements
  | InsufficientMaterial

-- Possible results of a player action result.
data PlayerActionOutcome
  = GameStarted ChessStateRepresentation [ExecutableAction]
  | PlayerMoved ChessStateRepresentation [ExecutableAction] Bool
  | WonByCheckmate ChessStateRepresentation Color
  | LostByResignation ChessStateRepresentation Color
  | Draw ChessStateRepresentation Color DrawType

type ExecutableAction = (PlayerActionRepresentation, () -> PlayerActionOutcome)

data PlayerAction
  = MovePiece Ply [Ply] Bool
  | Resign
  | AcceptDraw

representation outcome
  | Draw (repr, _, _) = repr
  | LostByResignation (repr, _) = repr
  | WonByCheckmate (repr, _) = repr
  | GameStarted (repr, _) = repr
  | PlayerMoved (repr, _, _) = repr

actions outcome
  | Draw _ = []
  | LostByResignation _ = []
  | WonByCheckmate _ = []
  | GameStarted (_, availableActions) = availableActions
  | PlayerMoved (_, availableActions, _) = availableActions

module ExecutableAction =
    let action = fst
    let executefn = snd

let representation game =
    ChessStateRepresentation {
        playerInTurn = turn game
        whitePlayerCastlingRights = whitePlayerCastlingRights game
        blackPlayerCastlingRights = blackPlayerCastlingRights game
        pawnCapturableEnPassant = pawnCapturableEnPassant game
        board = Array2D.init 8  8 (fun r f ->
            game @@@ (File.fromInt f, Rank.fromInt r)
        )
        moves = List.rev game.plies
        pliesWithoutPawnOrCapture = game.pliesWithoutPawnOrCapture
        numberOfMoves = game.numberOfMoves
        isCheck = isCheck game.playerInTurn game
    }
