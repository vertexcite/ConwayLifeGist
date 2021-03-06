module Life where

import Data.Set (Set)
import qualified Data.Set as S

type Cell = (Int, Int)

type World = Set Cell

candidates :: World -> Set Cell
candidates w =  S.fold S.union S.empty (S.map explode w)

neighbourCount :: Cell -> World -> Int
neighbourCount c w = S.size $ S.filter (\x -> x `S.member` explode c) w

births :: World -> Set Cell
births w = S.filter (\c -> neighbourCount c w == 3) (candidates w)

survivors :: World -> Set Cell
survivors w = S.filter (\c -> neighbourCount c w `elem` [2,3]) w

next :: World -> World

next = nextCorrect -- Comment out the code on either this line or the line below
-- next = nextBroken -- Uncomment code on this line to intentionally break things to see testing in action (though that depends on QuickCheck being lucky enough to randomly choose a case that fails)

nextCorrect :: World -> World
nextCorrect w = births w `S.union` survivors w

nextBroken :: World -> World
nextBroken w = deadpixel `S.insert` births w `S.union` survivors w where deadpixel = (15,12)

explode :: Cell -> Set Cell
explode (x,y) = S.fromList [(x+dx,y+dy) | dx <- range, dy <- range, (dx,dy) /= (0,0)]
  where range = [-1..1]


