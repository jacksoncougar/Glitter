{-
author: jackson c. wiebe
date:   26 Feb 2018
last:   ''
-}

import Debug.Trace
import Data.List
import Data.Maybe

import Polyomino
import Types

data Board  = Board
  { bounds::Rectangle
  , locs::[Location]
  , tokens::[Char]
  }

data Problem = Problem { board::Board, polys::[Polyomino] }
  deriving Show

data Solution = Solution { solution_board::Board}
  deriving Show

instance Show Board where
  show (Board rec _ labels)
    = let loc = locations rec in
        '\n' : show' (head loc) loc labels
    where
      show' :: Location -> [Location] -> [Char] -> String
      show' x (p:ps) (q:qs) | col x == col p = q : show' x ps qs
                            | otherwise = '\n' : q : show' p ps qs
      show' _ _ _ = ""

makeBoard :: Int -> Int -> Board
makeBoard x y  = Board (Rectangle 0 0 x y) [
  (i,j) | j <- [0..(y-1)],
          i <- [0..(x-1)]
                       ] (replicate (x*y) '.' )

polomino_4x2_c = createPolyomino [(0,1),(0,0),(1,0),(2,0),(3,0),(3,1)]
polomino_4x2_c' = createPolyomino [(0,0),(0,1),(1,1),(2,1),(3,1),(3,0)]
polomino_2x2_square = createPolyomino [(0,0),(0,1),(1,0),(1,1)]
testBoard = makeBoard 4 4
testProblem = Problem testBoard testPolys
testPolys = [ flipxy $ polomino_4x2_c 'A'
            , polomino_2x2_square 'B'
            , polomino_4x2_c' 'D']



fits :: Board -> Polyomino -> Bool
fits (Board{locs=bs}) (Polyomino{ parts=cs }) =
  and $ map (flip elem bs) cs

-- Translate the polyomino to given location
trans :: Polyomino -> Location -> Polyomino
trans p@(Polyomino{ parts=xs }) loc =
  p { parts = map (add loc) xs }


-- Places the polyomino on the board
place :: Board -> Polyomino -> Board
place b@(Board{ locs=xs }) (Polyomino{parts=ps, token=token}) =
  let b' = placeTokens b token ps in
      b'{ locs = xs \\ ps }
  
placeTokens :: Board -> Token -> [Location] -> Board
placeTokens b@Board{ tokens=qs } token (x:xs)
  =  placeTokens (setLabel b x token) token xs
placeTokens b _ [] = b

setLabel :: Board -> Location -> Char -> Board
setLabel board i label
  = board{ tokens =
           setLabel' (indexOf i (widthOf (bounds board)))
           label (tokens board)
         }
  where setLabel' :: Int -> Char -> [Char] -> [Char]
        setLabel' i label (x:xs) | i == 0 = label : xs
                                 | i > 0 = x : setLabel' (i-1) label xs
                                 | otherwise = []
                                 
        indexOf :: Location -> Int -> Int
        indexOf (row, col) width = row + col * width

        widthOf :: Rectangle -> Int
        widthOf (Rectangle _ _ w _) = w
                               

add :: Location -> Location -> Location
add (a,b) (c,d) = (a + c, b + d)

orients :: Polyomino -> [Polyomino]
orients p = let p' = flipv p;
                a  = rotate p;
                b  = rotate a;
                c  = rotate b;
                a' = flipv a;
                b' = flipv b;
                c' = flipv c in
              nub [p,a,b,c,p',a',b',c']
            where
              rotate = flipv . flipxy


places' :: Board -> Polyomino -> [Polyomino]
places' b p =
  let os = orients p;         --[Polyomino]
      xs = (map (places b) os)--[[Location]] 
      in
    peel os xs
  where peel (o:os) (x:xs) = map (trans o) x ++ peel os xs
        peel [] _ = []
    
      


  

places :: Board -> Polyomino -> [Location]
places board poly =  filter (fits board . trans poly) $ locs board

solve' :: Board -> [Polyomino] -> [Polyomino]-> Maybe Solution
solve' b (l:ls) (p:q:ps) = do
  let board'   = (place b $ l);    
      solution = solve' board' (places' board' q) (q:ps)
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



testSolve = solve' testBoard (places' testBoard $ head testPolys) testPolys
