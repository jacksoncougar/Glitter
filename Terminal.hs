{-
author: Jackson C. Wiebe
date:   March 1 2018
-}

module Terminal (color, clear, ansi_colors) where

color :: Int -> String -> String
color i s = "\27[97;" ++ (show i) ++ "m" ++ s ++ "\27[0m"

ansi_colors :: [Int]
ansi_colors = cycle [ x | x <- [40..47]++[100..107]]

clear :: String
clear = "\27[1;H\27[2;J"
