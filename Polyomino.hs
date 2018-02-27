{-
author: jackson c. wiebe
date:   26 Feb 2018
last:   ''
-}

module Polyomino
  ( Polyomino (Polyomino, token, parts, width)
  , createPolyomino
  , flipxy
  , flipv
  ) where

import Types
import Data.List
data Polyomino = Polyomino { parts::[Location]
                           , width::Int
                           , height::Int
                           , token::Token
                           }
                 deriving Eq

createPolyomino :: [Location] -> Token -> Polyomino
createPolyomino xs token =
  let points = unzip xs;
      width  = (maximum $ fst points) + 1;
      height = (maximum $ snd points) + 1 in
  Polyomino xs width height token

flipxy :: Polyomino -> Polyomino
flipxy p@(Polyomino{ parts=(ps), width=w, height=h }) =
  p{ parts=sort $ flip' ps, width=h, height=w }

  where flip' :: [Location] -> [Location]
        flip' ((a,b):xs) = (b,a):flip' xs
        flip' [] = []

flipv :: Polyomino -> Polyomino
flipv p@(Polyomino{ parts=ps, width=w }) =
  p{ parts = sort $ map (flip flipv' w) ps  }

  where flipv' :: Location -> Width -> Location
        flipv' (x,y) w = ((w - 1 - x),y)
                  
instance Show Polyomino where
  show (Polyomino{parts=ps, token=t, width=w, height=h}) =

    intercalate "\n" . splitEvery w $ setTokens [] t (sort ps) w

    where
      setTokens :: [Token] -> Token -> [Location] -> Int -> [Token]
      setTokens vs a ((x,y):xs) w = do
        let vs' = setAt (x + y * w) a ' ' vs
        setTokens vs' a xs w
      setTokens vs _ [] _ = vs

      splitEvery _ [] = []
      splitEvery n list =
        let (first,rest) = splitAt n list in
          first : (splitEvery n rest)

      setAt :: Int -> Token -> Token -> [Token] ->[Token]
      setAt 0 a _ (_:xs) = a : xs
      setAt 0 a b [] = [a]
      setAt i a b (x:xs) | i > 0 = x : setAt (i-1) a b xs
                         | otherwise = []
      setAt i a b [] | i > 0 = b : setAt (i-1) a b []
                     | otherwise = []

                     






          
