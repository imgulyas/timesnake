module Game.Position
  ( Cardinal(..)
  , Position(..)
  ) where


data Cardinal = N | E | S | W

data Position = Position { getX :: Word
                         , getY :: Word
                         }
