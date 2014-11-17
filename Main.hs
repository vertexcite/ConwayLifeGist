{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.All
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad
import Data.Maybe

import Life


prop_rules :: Int -> Int -> [(Int, Int)] -> Bool
prop_rules width height l = and $ S.toList $ S.map rules allCells
  where 
    w = S.fromList (massageList width height l)
    w' = next w
    allCells = candidates $ blockOfCells width height `S.union` w `S.union` w'
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
massageList width height = map (\(x,y) -> (if width == 0 then x else x `mod` width, if height == 0 then y else y `mod` height))

blockOfCells :: Int -> Int -> Set Cell
blockOfCells width height = S.fromList [(x,y) | x <- [0..width-1], y <- [0..height-1]]

-- Using QuickCheck's arbitrary
-- Note that prop_rules2 only tests over a fixed grid, whereas prop_rules allows QuickCheck to define the grid bounds.
-- This highlights QuickCheck's power, where it homes in on the simplest case.
-- To demonstrate this, force the implementation to have a "dead pixel" outside the usual bounds, e.g. by 
-- changing next as follows
-- next w = deadpixel `S.insert` births w `S.union` survivors w where deadpixel = (70,37)
newtype WorldArb = WorldArb { unworld :: World} deriving Show

width', height' :: Int
width' = 10
height' = 10

instance Arbitrary WorldArb where
  arbitrary = do
    maybes <- forM (S.toList (blockOfCells width' height')) $ \c -> do
      alive <- choose (False, True)
      return $ if alive then Just c else Nothing
    return . WorldArb . S.fromList . catMaybes  $ maybes

prop_rules2 :: WorldArb -> Bool
prop_rules2 wa = prop_rules width' height' w
  where w = S.toList . unworld $ wa



--------------------------------------------------------------------------
-- main

return []
main = $quickCheckAll

--------------------------------------------------------------------------
-- the end.
