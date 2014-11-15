{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Life where

import Test.QuickCheck
import Test.QuickCheck.All
import Data.Set (Set)
import qualified Data.Set as S

type Cell = (Int, Int)

type World = Set Cell

candidates :: Set Cell -> Set Cell
candidates w =  S.fold S.union S.empty (S.map explode w)

neighbourCount :: Cell -> Set Cell -> Int
neighbourCount c w = S.size $ S.filter (\x -> x `S.member` explode c) w

births :: Set Cell -> Set Cell
births w = S.filter (\c -> neighbourCount c w == 3) (candidates w)

survivors :: Set Cell -> Set Cell
survivors w = S.filter (\c -> neighbourCount c w `elem` [2,3]) w

next :: Set Cell -> Set Cell
next w = births w `S.union` survivors w

width, height :: Int
width = 10
height = 10

allCells :: Set Cell
allCells = S.fromList [(x,y) | x <- [0..width-1], y <- [0..height-1]]

explode :: Cell -> Set Cell
explode (x,y) = S.fromList [(x+dx,y+dy) | dx <- range, dy <- range, (dx,dy) /= (0,0)]
  where range = [-1..1]


prop_rules :: [(Int, Int)] -> Bool
prop_rules l = and $ S.toList $ S.map rules allCells
  where 
    w = S.fromList (massageList l)
    w' = next w
    rules c = or [rule_overCrowding, rule_lonely, rule_survive, rule_born, rule_empty]
      where
        rule_overCrowding = alive && not alive' && n > 3
        rule_lonely       = alive  && not alive' && n < 2
        rule_survive      = alive && alive' && n `elem` [2,3]
        rule_born         = not alive && alive' && n == 3
        rule_empty        = not alive && not alive' && n /= 3
        n = neighbourCount c w
        alive  = c `S.member` w
        alive' = c `S.member` w'

-- Forces random elements into the grid of dimensions width x height  
massageList :: [(Int, Int)] -> [(Int, Int)]
massageList = map (\(x,y) -> (x `mod` width, y `mod` height))






--------------------------------------------------------------------------
-- main

return []
main = $quickCheckAll

--------------------------------------------------------------------------
-- the end.
