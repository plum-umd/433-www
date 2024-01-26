{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Game where

import Control.Applicative ((<|>))
import Control.Monad (guard)

import Control.Lens hiding ((<|), (|>))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State

import System.Random (Random(..), newStdGen)

import Data.Maybe (fromMaybe, isJust, fromJust, isNothing)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Ix(range)

-- Types

type Loc = (Int, Int)

data Player = X | O
  deriving (Eq, Show)

type Board = Map Loc Player

data Game = Game
  { _player :: Player
  , _cursor :: Loc
  , _board :: Board
  , _done  :: Bool
  , _won   :: Maybe ([Loc], Player)
  } deriving (Show)

data Direction = North | South | East | West
  deriving (Eq, Show)

makeLenses ''Game

-- Constants

height, width :: Int
height = 3
width  = 3

initBoard :: Board
initBoard = Map.empty

traversals :: [[Loc]]
traversals = [[(x,y) | y <- [0..height-1]] | x <- [0..width-1 ]] ++
             [[(x,y) | x <- [0..width-1 ]] | y <- [0..height-1]] ++
             [[(x,x) | x <- [0..width-1]]] ++
             [[(x,height-x-1) | x <- [0..width-1]]]

squares :: [Loc]
squares = range ((0,0), (width-1, height-1))

-- Functions

full :: Game -> Bool
full g = all (\l -> isJust (g ^. board . at l)) squares 

isWinning :: Game -> [Loc] -> Player -> Bool
isWinning g ls p = all (\l -> Just p == (g ^. board . at l)) ls

checkForWin :: Game -> Maybe ([Loc],Player)
checkForWin g =
  case filter (\(ls,p) -> isWinning g ls p)
              [(ls, p) | ls <- traversals, p <- [X,O]] of
    [] -> Nothing
    (x:_) -> Just x
    
move :: Direction -> Game -> Game
move North g = g & cursor . _2 %~ (\y -> (y - 1) `mod` height)
move South g = g & cursor . _2 %~ (\y -> (y + 1) `mod` height)
move East  g = g & cursor . _1 %~ (\x -> (x + 1) `mod` width)
move West  g = g & cursor . _1 %~ (\x -> (x - 1) `mod` width)

next :: Player -> Player
next X = O
next O = X

register :: Game -> Game
register g
  | g ^. done = g
  | isJust (g ^. board . at (g ^. cursor)) = g
  | otherwise = g & board  %~ Map.insert (g ^. cursor) (g ^. player)
                  & player %~ next
--    g & board  %~ Map.insertWith (flip const) (g ^. cursor) (g ^. player)

             
-- | Step forward in time
step :: Game -> Game
step g
  | g ^. done = g
  | isJust checkWin = g & done .~ True
                        & won  .~ checkWin
  | full g = g & done .~ True
  | otherwise = g 
  where checkWin = checkForWin g

{-
type Score = Int

score :: Game -> Player -> Score
score g p =
  case g ^. won of
    Just (_,p') -> if p == p' then 1 else -1
    Nothing -> 0

moves :: Game -> Player -> [Game]
moves g p =
  map (\l -> minimax (register (g & cursor .~ l)) p) 
      (filter (\l -> isNothing (g ^. board . at l)) squares)
                   
minimax :: Game -> Player -> Score
minimax g p 
  | g ^. done = score g p
  | otherwise =
    let agg = if g ^. player == p then maximum else minimum
    in agg (moves g p)
-}

-- Initialization
initGame :: IO Game
initGame = 
  return $ Game { _cursor = (0,0)
                , _player = X
                , _board = initBoard
                , _done = False
                , _won  = Nothing }
