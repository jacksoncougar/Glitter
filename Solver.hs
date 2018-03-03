{-
author: Jackson C. Wiebe
date:   March 1 2018
last:   ''
-}

module Solver
  ( Problem(..)
  , solve
  ) where


import Debug.Trace
import Data.List
import Data.Maybe
import Polyomino
import Types
import Board
import Terminal

data Problem = Problem { board::Board, polys::[Polyomino] }
  deriving Show

solve :: Problem -> Maybe Board

solve (Problem b@Board{bounds=(_,_,w,h)} ps) = do
  let bs = (0,0,0,0)
  let (sp:sps) = sort ps
  solve' b bs (places b bs sp) (sp:sps) []

-- # SUB-SOLVER # -- 
solve' :: Board -> Bounds
        -> [Polyomino] -- placements of current polyomino
        -> [Polyomino] -- remaining pieces to place
        -> [Polyomino] -- failed pieces
        -> Maybe Board

-- dummy for showing intermediate steps
solve' b _ _ _ _ | trace ( clear ++ "Searching: " ++ show b) False = Nothing

solve' b bs ps (r:rs) ts
   -- subcess case:
  | (filled b bs) = do
      let bs' = grow b bs
      let ps' = places b bs' r
      --retry with new bounds
      solve' b bs' ps' ((r:rs)++ts) []

-- tried all places for current piece (more pieces remain)
solve' b bs [] (r:q:rs) fs = 
  let ps' = places b bs q in
    solve' b bs ps' (q:rs) (r:fs) -- try with another polyomino

-- normal reduction:
solve' b bs (p:ps) (r:q:rs) fs = do
  -- place the current piece
  let b' = place b p
  let ps' = places b' bs q
  let solution = solve' b' bs ps' ((q:rs)++fs) [] -- try next piece
  case solution of
    Nothing -> solve' b bs ps (r:q:rs) fs -- try next placement
    _ -> solution

-- reduction when on last untried piece:
solve' b bs (p:ps) (r:rs) (f:fs) = do
  -- place the current piece
  let b' = place b p
  let ps' = places b' bs f
  solve' b' bs ps' (f:fs) [] -- try next piece

-- solving last piece
solve' b bs (p:ps) (r:rs) [] = do
  -- place last piece...
  let b' = place b p
  Just b'

-- tried all pieces and nothing fit
solve' _ _ [] _ _ = Nothing

-- grows the search space of the board
grow :: Board -> Bounds -> Bounds
grow b _ =
  let (x,y) = head $ sort $ locs b in
    (x,y,1,1) -- just return the next open place...

