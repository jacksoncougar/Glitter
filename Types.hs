{-
author: jackson c. wiebe
date:   26 Feb 2018
last:   ''
-}


module Types
  ( Width
  , Height
  , Rectangle(..)
  , Location
  , Token
  , Bounds(..)
  , row
  , col
  , locations)
where

type Width = Int
type Height = Int

data Rectangle = Rectangle Int Int Width Height
  deriving Show

type Location = (Int, Int)

type Token = Char

type Bounds = (Width, Height)

row :: Location -> Int
row x = fst x

col :: Location -> Int
col x = snd x

locations :: Bounds -> [Location]
locations (width,height)
  = [(i, j) | j <- [0..(height-1)], i <- [0..(width-1)]]
