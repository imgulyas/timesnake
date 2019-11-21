module Game.Snake(
  Snake(..)
  ) where

import Data.List.NonEmpty
import Game.Position

data Snake = Snake { getBody :: NonEmpty Position
                   , getOri :: Cardinal
                   }
