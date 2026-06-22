module Plies where

import CoreTypes

data PlyType
  = MoveType
  | CaptureType
  | EnPassantType
  | CastleKingSideType
  | CastleQueenSideType
  | MoveAndPromoteType
  | CaptureAndPromoteType
  deriving (Eq)

checkPly (ply, restOfPlies, drawOffer) =
  PlyOutput
    { ply = ply,
      restOfPlies = restOfPlies,
      isCheck = True,
      drawOffer = drawOffer
    }

regularPly (ply, restOfPlies, drawOffer) =
  PlyOutput
    { ply = ply,
      restOfPlies = restOfPlies,
      isCheck = False,
      drawOffer = drawOffer
    }
