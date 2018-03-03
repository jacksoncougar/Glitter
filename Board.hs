module Board
  ( Board(..)
  , makeBoard
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
  , tokens::[Token]
  }

instance Show Board where
  show (Board rec _ labels)
    = let loc = locations rec in
        concat $ "\n" : show' (head loc) loc labels
    where
      show' :: Location -> [Location] -> [Token] -> [Token]
      show' x (p:ps) (q:qs) | col x == col p = q : show' x ps qs
                            | otherwise = "\n" : q : show' p ps qs
      show' _ _ _ = [""]

makeBoard :: Int -> Int -> Board
makeBoard x y  = Board (0,0, x, y) [
  (i,j) | j <- [0..(y-1)],
          i <- [0..(x-1)]
                       ] (replicate (x*y) ". " )
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

    setLabel :: Board -> Location -> Token -> Board
    setLabel board i label
      = board{ tokens =
           setLabel' (indexOf i (widthOf (bounds board)))
           label (tokens board)
         }
    setLabel' :: Int -> Token -> [Token] -> [Token]
    setLabel' i label (x:xs) | i == 0 = label : xs
                             | i > 0 = x : setLabel' (i-1) label xs
                             | otherwise = []
                                 
    indexOf :: Location -> Int -> Int
    indexOf (row, col) width = row + col * width

    widthOf :: Bounds -> Int
    widthOf (_,_,w,_) = w
                               
places :: Board -> Bounds -> Polyomino -> [Polyomino]
places b bs p =
  let os = orientations p;
      xs = [ xs | xs <- concat $ map (places' b bs) os ] in
    xs
  where
    peel (o:os) (x:xs) = map (move o) x ++ peel os xs
    peel [] _ = []
    places' :: Board -> Bounds -> Polyomino -> [Polyomino]
    places' board bs poly = do
      let xs = locs board
      let xs' = locations bs
      let ps = filter (fits board) $ concat $ map (move' poly) xs
      sort $ nub $ filter (or . map (flip elem xs') . parts) $ ps
  


filled :: Board -> Bounds -> Bool
filled Board{locs=xs} bounds =
  let qs = locations bounds in
  qs \\ xs == qs
