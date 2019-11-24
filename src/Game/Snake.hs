{-# LANGUAGE TemplateHaskell #-}

module Game.Snake( Snake(..)
                 , stepSnake
                 , body
                 ) where

import           Control.Lens
import           Data.List.NonEmpty
import           Game.Position

data Snake = Snake { _body        :: NonEmpty Position
                   , _orientation :: Cardinal
                   , _id          :: Int
                   } deriving (Show)
makeLenses ''Snake

moveHead :: Cardinal -> Position -> Position
moveHead N = over y (subtract 1)
moveHead S = over y (+1)
moveHead W = over x (subtract 1)
moveHead E = over x (+1)

moveBody :: Cardinal -> NonEmpty Position -> NonEmpty Position
moveBody o (sh :| [])        = moveHead o sh :| []
moveBody o (sh :| [_])       = moveHead o sh :| [sh]
moveBody o (sh :| ht : rest) = moveHead o sh :| sh : Prelude.init rest

stepSnake :: Snake -> Snake
stepSnake s = over body (moveBody $ _orientation s) s
