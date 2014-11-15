{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.All
import Data.Set (Set)
import qualified Data.Set as S

import Life


prop_rules :: Int -> Int -> [(Int, Int)] -> Bool
prop_rules width height l = and $ S.toList $ S.map rules allCells
  where 
    w = S.fromList (massageList width height l)
    w' = next w
    allCells = candidates $ blockOfCells width height
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
massageList :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
massageList width height = map (\(x,y) -> (x `mod` width, y `mod` height))

blockOfCells :: Int -> Int -> Set Cell
blockOfCells width height = S.fromList [(x,y) | x <- [0..width-1], y <- [0..height-1]]





--------------------------------------------------------------------------
-- main

return []
main = $quickCheckAll

--------------------------------------------------------------------------
-- the end.
