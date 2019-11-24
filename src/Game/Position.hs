{-# LANGUAGE TemplateHaskell #-}

module Game.Position
  ( Cardinal(..)
  , Position(..)
  , x
  , y
  ) where

import           Control.Lens

data Cardinal = N | E | S | W deriving (Show)

data Position = Position { _x, _y :: Int
                         } deriving (Show, Eq, Ord)
makeLenses ''Position
