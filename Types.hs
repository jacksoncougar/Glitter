{-
author: Jackson C. Wiebe
date:   March 1 2018
-}


module Types
  ( Width
  , Height
  , Location
  , Token
  , Bounds(..)
  , row
  , col
  , locations)
where

type Width = Int
type Height = Int

type Location = (Int, Int)

type Token = String

type Bounds = (Int, Int, Width, Height)

row :: Location -> Int
row x = fst x

col :: Location -> Int
col x = snd x

locations :: Bounds -> [Location]
locations (x,y,width,height)
  = [(x+i, y+j) | j <- [0..(height-1)], i <- [0..(width-1)]]
