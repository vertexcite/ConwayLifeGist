module Main where

import Life
import Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main = print $ take 5 $ iterate next (S.fromList [(-1,0),(0,0),(1,0)])
