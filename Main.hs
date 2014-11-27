{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.All
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad
import Data.Maybe

import Life


prop_rules :: WorldArb -> Bool
prop_rules wa = and $ S.toList $ S.map rules allCells
  where 
    w = world wa
    w' = next w
    allCells = candidates $ blockOfCells (width wa) (height wa) `S.union` w `S.union` w'
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

prop_rules2 :: Int -> Int -> [(Int, Int)] -> Bool
prop_rules2 width' height' w = prop_rules wa
  where
    mw = massageList width' height' w
    wa = WorldArb {world = S.fromList mw, width = width', height = height'}

-- Forces random elements into the grid of dimensions width x height  
massageList :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
massageList w h = map (\(x,y) -> (if w == 0 then x else x `mod` w, if h == 0 then y else y `mod` h))


blockOfCells :: Int -> Int -> Set Cell
blockOfCells w h = S.fromList [(x,y) | x <- [0..w-1], y <- [0..h-1]]

-- Using QuickCheck's arbitrary
-- Note that prop_rules2 only tests over a fixed grid, whereas prop_rules allows QuickCheck to define the grid bounds.
-- This highlights QuickCheck's power, where it homes in on the simplest case.
-- To demonstrate this, force the implementation to have a "dead pixel" outside the usual bounds, e.g. by 
-- changing next as follows
-- next w = deadpixel `S.insert` births w `S.union` survivors w where deadpixel = (70,37)
data WorldArb = WorldArb { world :: World, width :: Int, height :: Int } deriving Show

instance Arbitrary WorldArb where
  arbitrary = do
    width' <- arbitrary
    height' <- arbitrary
    maybes <- forM (S.toList (blockOfCells width' height')) $ \c -> do
      alive <- choose (False, True)
      return $ if alive then Just c else Nothing
    return WorldArb {world = S.fromList . catMaybes $ maybes, width = width', height = height'}




--------------------------------------------------------------------------
-- main

return []
main = $quickCheckAll

--------------------------------------------------------------------------
-- the end.
