-- CPSC 312 - 2016 - Games in Haskell
module Minimax where

-- To run it, try:
-- ghci
-- :load Minimax

import MagicSum

----   Determining the best move  ---
minimax:: Game -> State -> (Action, Double)
-- minimax game state   =>  (move,value_to_player)
-- precondition: there are some moves that are available
minimax game st  =
      argmax (\ amove -> value game (game amove st)) avail
      where State _ avail = st

-- value game result  = value  for current player after result
value:: Game -> Result -> Double
value _  (EndOfGame val _) = val
value game (ContinueGame st) =  - snd (minimax game st)   -- (value,move) for next player
                 -- value for current player is negative of value of the other player

-- to find the best opening move
-- minimax magicsum magicsum_start

--Try (for MagicSum)
as lst = [Action i | i <- lst]     -- make it easier to type
-- minimax magicsum (State (as [8,5,4], as [1,2,6,9])  (as [3,7]))
-- minimax magicsum (State (as [8,5], as [1,2])  (as [3,4,6,7,9]))
-- minimax magicsum (State (as [1,2], as [4,5,8])  (as [3,4,6,7,9]))
-- minimax magicsum (State (as [1,2,4,5], as [3,7,8,9])  (as [6]))
-- minimax magicsum (State (as [1,2,4,5], as [3,6,8,9])  (as [7]))
-- minimax magicsum (State (as [1,2,5], as [3,6,8,9])  (as [4,7]))
-- minimax magicsum (State (as [1,5], as [3,8])  (as [2,4,6,7,9]))
-- minimax magicsum (State (as [1], as [5,8]) (as [2,3,4,6,7,9])))
-- minimax magicsum (State (as [5,8], as [1,2])  (as [3,4,6,7,9]))
-- minimax magicsum (State (as [5], as [3,8]) (as [1,2,4,6,7,9]))
-- minimax magicsum (State ([], [])  (as [1..9])


mm_player:: Game -> Player
mm_player game state = fst ( minimax game state)


-- argmax f lst  = (e, f e) for e <- lsts where f e is maximal
--  Note that this does not require the elements of lst to be comparable
-- like  max[(f e,e) <- e in lst] but where only the first elements of pairs are compared in the max.
argmax :: Ord v => (e -> v) -> [e] -> (e,v)
argmax f [e] = (e, f e)
argmax f (h:t) 
   | fh > ft = (h,fh)
   | otherwise = (bt, ft)
   where
      (bt,ft) = argmax f t
      fh = f h

--- after surfing the web, I found this is the standard argmaxWithMax
--  http://hackage.haskell.org/package/list-extras-0.4.1.4/docs/Data-List-Extras-Argmax.html

-- Test case:
-- argmax (\x -> 5- (x-2)^2) [0..10]
-- argmax (\x -> 1 + 4*x - x^2) [0..10]

-- another implementation
argmax2 :: Ord v => (e -> v) -> [e] -> (e,v)
argmax2 f (h:t) =
   foldr (\ e (et,vt) -> let fe= f e in
                         if (fe > vt) then (e,fe) else (et,vt))
         (h, f h) t
   