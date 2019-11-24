{-# LANGUAGE TemplateHaskell #-}

module Game.World ( World(..) ) where

import           Control.Lens
import           Control.Monad.Reader
import           Data.List.NonEmpty as NE
import           Data.Set as Set
import           Game.Position
import           Game.Snake

type Id = Word
data Turn = Left | Right deriving (Show, Eq)

data WorldEvent = AppleAdded Position
                | AppleRemoved Position
                | SnakeTurned Id Turn --TODO figure out how to make left and right turns per player exclude each other
                | SnakeGrew Id
                | Collision Position
                | SnakeDied Id
                deriving (Show, Eq)

data GameConfig = GameConfig { _worldSize     :: Position
                             , _maxChangeSets :: Word
                             }
makeLenses ''GameConfig

data World = World { _snakes      :: [Snake]
                   , _apples      :: Set Position
                   , _collisions  :: Set Position
                   , _dyingSnakes :: [(Snake, Word)]
                   } deriving (Show)
makeLenses ''World

stepSnakes :: World -> World
stepSnakes = over snakes $ fmap stepSnake

type Game a = Reader GameConfig a

occupiedPositions :: World -> Set Position
occupiedPositions w = let
                        bodies = toListOf (snakes . traverse . body) w
                      in
                        foldl1 union $ Set.fromList . NE.toList <$> bodies

checkCollisions :: World -> Game ([WorldEvent], World)
checkCollisions oldWorld = undefined

-- updateWorld :: World -> Set WorldEvent -> Game World
-- updateWorld old changes = do
--                           config <- ask
--                           return old

-- data GameState = GameState { _changes :: [Set WorldEvent] -- head is the latest set of changes
--                            , _seed    :: World
--                            }
-- makeLenses ''GameState



-- worldStep :: Game World -> Set WorldEvent -> Game World
-- worldStep gw changes = do
--           world <- gw
--           conf <- ask

--           pure world

-- renderWorldAt :: GameState -> Int -> Maybe World
-- renderWorldAt gs ticks  = let
--                             len = length $ _changes gs
--                           in
--                             if  ticks < 0 || ticks > (len+1) then Nothing
--                             else Just $ Prelude.foldl worldStep (_seed gs) (Prelude.take (len - ticks) . reverse $ _changes gs)

-- replaceSeed :: GameState -> Int -> Maybe GameState
-- replaceSeed gs ticks = let
--                          maybeNewSeed = renderWorldAt gs ticks
--                          makeNewState new = GameState { _seed = new
--                                                       , _changes = reverse . Prelude.drop ticks . reverse $ _changes gs
--                                                       }
--                        in makeNewState <$> maybeNewSeed

