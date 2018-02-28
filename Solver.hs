{-
author: jackson c. wiebe
date:   26 Feb 2018
last:   ''
-}



module Solver
  ( Problem(..)
  , Solution(..)
  , solve
  , solve'''
  ) where


import Debug.Trace
import Data.List
import Data.Maybe
import Polyomino
import Types
import Board

data Problem = Problem { board::Board, polys::[Polyomino] }
  deriving Show

data Solution = Solution { solved_board::Board}
  deriving Show

solve :: Problem -> Maybe Solution
solve Problem{board=b, polys=polys@(p:_)} =
  let bounds = (1,1) in
    solve' b  (places b  p) polys 
  where
    solve' :: Board -> [Polyomino] -> [Polyomino]-> Maybe Solution
    solve' b (l:ls) (p:q:ps) = do
      let board'   = (place b $ l);    
          solution = solve' board' (places board' q) (q:ps)
      case solution of 
        Nothing -> solve' b ls (p:q:ps) -- solve for next location
        _       -> solution             -- return solution 

    solve' b (l:ls) (p:ps) = 
      let board' = (place b $ l);
          solution = solve' board' [] ps in
        case solution of 
          Nothing -> solve' b ls (p:ps)   -- solve for next location
          _       -> solution             -- return solution 

    solve' board _ [] = Just $ Solution board
    solve' b [] _ = Nothing

-- # SUB-SOLVER # -- 
solve'' :: Board -> Bounds
            -> [Polyomino] -- placements of current polyomino
            -> [Polyomino] -- remaining pieces to place
            -> [Polyomino] -- failed pieces
            -> Maybe Board

solve''' :: Problem -> Maybe Board
solve''' (Problem b (p:ps)) =
  let result = solve'' b (1,1) (places b p) ps [] in
  case result of
    Nothing -> Nothing
    _ -> result

solve'' b bs (p:ps) (r:rs) fs = do
  let b' = place b p
  let solution = solve'' b' bs (places b' r) rs fs 
  case solution of
    Nothing -> solve'' b bs ps (r:rs) fs
    _ -> solution
      
  -- can't place current piece filled
solve'' b bounds [] (r:q:rs) fs
  | filled b bounds = Just b
  | otherwise =
      solve'' b bounds (places b q) rs (r:fs)

solve'' b bounds [] (r:rs) fs
  | filled b bounds = Just b
  | otherwise = Nothing
                      
    
    -- all placements tried and all pieces tried
solve'' _ _ [] [] _ = Nothing 
            
