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
  , solve''
  , places''
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
          solution = solve' board' (places'' board' (bounds board') q) (q:ps)
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



debug b bs x ps rs ts = (trace $
                     "\nbounds: " ++ show bs ++ 
                    "\nboard: " ++ show b)

                   
-- # SUB-SOLVER # -- 
solve'' :: Board -> Bounds
        -> [Polyomino] -- placements of current polyomino
        -> [Polyomino] -- remaining pieces to place
        -> [Polyomino] -- failed pieces
        -> Maybe Board

solve''' :: Problem -> Maybe Board
solve''' (Problem b (p:ps)) =
  let bs = (0,0);
      result = solve'' b bs (places'' b bs p) (p:ps) [] in
  case result of
    Nothing -> Nothing
    _ -> result

eq (w,h) (w',h') = w==w' && h==h'

solve'' b bs ps rs ts | (debug b bs ' ' ps rs ts) False = Nothing

solve'' b bs ps (r:rs) ts
   -- subcess case:
  | (eq bs $ bounds b) == False && (filled b bs) = do
      let bs' = grow b bs
      let ps' = places'' b bs' r
      --retry with larger bounds
      solve'' b bs' ps' ((r:rs)++ts) []

-- tried all places for current piece (more pieces remain)
solve'' b bs [] (r:q:rs) fs = 
  let ps' = places'' b bs q in
    solve'' b bs ps' (q:rs) (r:fs) -- try with another polyomino

-- reduction steps:
solve'' b bs (p:ps) (r:q:rs) fs = do
  -- place the current piece
  let b' = place b p
  let ps' = places'' b' bs q
  let solution = solve'' b' bs ps' ((q:rs)++fs) [] -- try next piece
  case solution of
    Nothing -> solve'' b bs ps (r:q:rs) fs -- try next placement
    _ -> solution

solve'' b bs (p:ps) (r:rs) (f:fs) = do
  -- place the current piece
  let b' = place b p
  let ps' = places'' b' bs f
  let solution = solve'' b' bs ps' (f:fs) [] -- try next piece
  case solution of
    Nothing -> Nothing
    _ -> solution


solve'' b bs (p:ps) (r:rs) [] = do
  -- place the current piece
  let b' = place b p
  if filled b' $ bounds b'
    then Just b'
    else solve'' b bs ps (r:rs) []

-- tried all pieces and nothing fit
solve'' _ _ [] _ _ = Nothing



  {--
  -- can't place current piece filled
solve'' b bs@(w,h) [] (r:q:rs) fs
  | filled b (bounds b) = Just b
  | filled b bs
  = solve'' b (grow b bs) (places'' b bs r) ((r:rs)++fs) []
  | otherwise =
      solve'' b bs (places'' b bs q) rs (r:fs)

solve'' b bs@(w,h) [] (r:rs) fs
  | filled b (bounds b) = Just b
  | filled b bs
  = solve'' b (w+1,h+1) (places'' b bs r) ((r:rs)++fs) []
  | otherwise = Nothing

    -- all placements tried and all pieces tried
solve'' b _ [] [] _ | filled b (bounds b) = Just b
                    | otherwise = Nothing 
     --}       
grow :: Board -> Bounds -> Bounds
grow b@Board{bounds=bs@(u,v)} bs'@(w,h)
  | w >= u =
    let h' = h + 1 in
      (w, minimum [h', snd bs])
  |otherwise =
     let w' = w + 1 in
       (minimum [w', fst bs], h){--
    let w' = w + 1;
      h' = h + 1;
      bs'' = (minimum [w', fst bs], minimum [h', snd bs]) in
    (trace $ "bounds: " ++ show bs'' ++ "\nboard: " ++ (show b))bs''
--}

places'' :: Board -> Bounds -> Polyomino -> [Polyomino]
places'' b bs p =
  let os = orientations p;
      xs = [ xs | xs <- concat $ map (places' b bs) os ] in
    xs
  where
    peel (o:os) (x:xs) = map (move o) x ++ peel os xs
    peel [] _ = []
    places' :: Board -> Bounds -> Polyomino -> [Polyomino]
    places' board bs@(w,h) poly = do
      let xs = locs board
      let xs' = locations bs
      let ps = filter (fits board) $ map (move poly) xs
      sort $ nub $ filter (or . map (flip elem xs') . parts) $ ps
  
