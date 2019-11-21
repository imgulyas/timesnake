module Game.World where

import Game.Snake

data World = MkWorld { getSnakes :: [Snake]
                     }
