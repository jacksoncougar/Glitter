module Board
  ( Board(..)
  , makeBoard
  , fits
  , place
  , places
  , filled
  ) where

import Types
import Polyomino
import Data.List

data Board  = Board
  { bounds::Bounds
  , locs::[Location]
  , tokens::[Char]
  }

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
makeBoard x y  = Board (0,0, x, y) [
  (i,j) | j <- [0..(y-1)],
          i <- [0..(x-1)]
                       ] (replicate (x*y) '.' )
fits :: Board -> Polyomino -> Bool
fits (Board{locs=bs}) (Polyomino{ parts=cs }) =
  and $ map (flip elem bs) cs

-- Places the polyomino on the board
place :: Board -> Polyomino -> Board
place b@(Board{ locs=xs }) (Polyomino{parts=ps, token=token}) =
  let b' = placeTokens b token ps in
      b'{ locs = xs \\ ps }
  where
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
    setLabel' :: Int -> Char -> [Char] -> [Char]
    setLabel' i label (x:xs) | i == 0 = label : xs
                             | i > 0 = x : setLabel' (i-1) label xs
                             | otherwise = []
                                 
    indexOf :: Location -> Int -> Int
    indexOf (row, col) width = row + col * width

    widthOf :: Bounds -> Int
    widthOf (_,_,w,_) = w
                               
places :: Board -> Polyomino -> [Polyomino]
places b p =
  let os = orientations p;  
      xs = (map (places' b) os) 
      in
    peel os xs
  where
    filter' :: [[Location]] -> (Int,Int) -> [[Location]]
    filter' (x:xs) bounds = filter'' x bounds : filter' xs bounds

    filter'' :: [Location] -> (Int,Int) -> [Location]
    filter'' xs (w,h) = filter (\(x,y) -> x <= w && y <= h) xs
    
    peel (o:os) (x:xs) = map (move o) x ++ peel os xs
    peel [] _ = []
    places' :: Board -> Polyomino -> [Location]
    places' board poly =
      filter (fits board . move poly) $ locs board

filled :: Board -> Bounds -> Bool
filled Board{locs=xs} bounds =
  let qs = locations bounds in
  qs \\ xs == qs
