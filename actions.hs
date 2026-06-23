module Actions
  ( newStandardChessGame,
    PlayerAction (..),
    PlayerActionOutcome (..),
    ChessStateRepresentation (..),
    Representable (..),
    actions,
  )
where

import Chess
import ChessState
import CoreTypes
import Data.Array
import Data.Function ((&))
import qualified Data.Map as Map
import Plies

data ChessStateRepresentation = ChessStateRepresentation
  { board' :: Array (Int, Int) Square,
    playerInTurn' :: Player,
    isCheck' :: Bool,
    whitePlayerCastlingRights' :: CastlingRights,
    blackPlayerCastlingRights' :: CastlingRights,
    pawnCapturableEnPassant' :: Maybe Coordinate,
    pliesWithoutPawnOrCapture' :: Int,
    moves' :: [PlyOutput],
    numberOfMoves' :: Int
  }

class Representable a where
  representation :: a -> ChessStateRepresentation

instance Representable ChessStateRepresentation where
  representation = id

data DrawType
  = Agreement
  | Stalemate
  | FiftyMovements
  | ThreefoldRepetition
  | FivefoldRepetition
  | SeventyFiveMovements
  | InsufficientMaterial
  deriving (Show)

-- Possible results of a player action result.
data PlayerActionOutcome
  = GameStarted ChessStateRepresentation [ExecutableAction]
  | PlayerMoved ChessStateRepresentation [ExecutableAction] Bool
  | WonByCheckmate ChessStateRepresentation Color
  | LostByResignation ChessStateRepresentation Color
  | Draw ChessStateRepresentation Color DrawType

data PlayerAction
  = MovePiece Ply [Ply] Bool
  | Resign
  | AcceptDraw

type ExecutableAction = (PlayerAction, () -> PlayerActionOutcome)

instance Representable PlayerActionOutcome where
  representation outcome =
    case outcome of
      Draw repr _ _ -> repr
      LostByResignation repr _ -> repr
      WonByCheckmate repr _ -> repr
      GameStarted repr _ -> repr
      PlayerMoved repr _ _ -> repr

actions outcome =
  case outcome of
    Draw {} -> []
    LostByResignation {} -> []
    WonByCheckmate {} -> []
    GameStarted _ availableActions -> availableActions
    PlayerMoved _ availableActions _ -> availableActions

initArray2d :: Int -> Int -> (Int -> Int -> a) -> Array (Int, Int) a
initArray2d rows cols f =
  listArray
    ((0, 0), (rows - 1, cols - 1))
    [f i j | i <- [0 .. rows - 1], j <- [0 .. cols - 1]]

instance Representable ChessState where
  representation game =
    ChessStateRepresentation
      { playerInTurn' = turn game,
        whitePlayerCastlingRights' = whiteCastlingRights game,
        blackPlayerCastlingRights' = blackCastlingRights game,
        pawnCapturableEnPassant' = ChessState.pawnCapturableEnPassant game,
        board' = initArray2d 8 8 (\r f -> game @@@ Coordinate (toEnum f, toEnum r)),
        moves' = reverse (ChessState.plies game),
        pliesWithoutPawnOrCapture' = ChessState.pliesWithoutPawnOrCapture game,
        numberOfMoves' = numberOfMoves game,
        isCheck' = isPlayerInCheck (turn game) game
      }

playerPlies game =
  let getCapabilities (sourceCoord, sourcePiece) =
        pieceCapabilities game (sourcePiece, sourceCoord)
      belongsToPlayer (_, Piece pieceColor _) = pieceColor == turn game
   in pieces game
        & Map.toAscList
        & filter belongsToPlayer
        & concatMap getCapabilities

executePlayerAction :: ChessState -> PlayerAction -> PlayerActionOutcome
executePlayerAction game playerAction =
  case playerAction of
    MovePiece ply restOfPlies drawOffer ->
      let newGame = nextGameState ply restOfPlies drawOffer game
       in getOutcomeFromNewBoard newGame
    Resign ->
      let repr = representation game
       in LostByResignation repr (opponent (turn game))
    AcceptDraw ->
      let repr = representation game
       in Draw repr (turn game) Agreement

getOutcomeFromNewBoard game =
  let repr = representation game
      safeHead l = case l of x : xs -> Just x; [] -> Nothing
      orElse a (Just b) = b
      orElse a Nothing = a
      drawOffered = plies game & safeHead & fmap drawOffer & orElse False
      result
        | drawOffered && (pliesWithoutPawnOrCapture game >= 50) =
            Draw repr (turn game) FiftyMovements
        | drawOffered && (repetitionCount game >= 3) =
            Draw repr (turn game) ThreefoldRepetition
        | drawOffered && (pliesWithoutPawnOrCapture game >= 75) =
            Draw repr (turn game) SeventyFiveMovements
        | drawOffered && (repetitionCount game >= 5) =
            Draw repr (turn game) FivefoldRepetition
        | otherwise =
            let playerPlies' = playerPlies game
                canMove = not (null playerPlies')
                result'
                  | canMove =
                      let actions = getExecutableActions playerPlies' game drawOffered
                       in case playerPlies' of
                            [] -> GameStarted repr actions
                            _ -> PlayerMoved repr actions drawOffered
                  | isPlayerInCheck (turn game) game =
                      WonByCheckmate repr (turn game)
                  | otherwise =
                      Draw repr (turn game) Stalemate
             in result'
   in result

getExecutableActions :: [Ply] -> ChessState -> Bool -> [ExecutableAction]
getExecutableActions plies game drawOffered =
  plies
    & concatMap (\ply -> [MovePiece ply plies False, MovePiece ply plies True])
    & (Resign :)
    & (\l -> if drawOffered then AcceptDraw : l else l)
    & fmap (makeNextExecutableAction game)

makeNextExecutableAction game playerAction =
  let executeFn () = executePlayerAction game playerAction
   in (playerAction, executeFn)

newStandardChessGame = getOutcomeFromNewBoard initialGameState
