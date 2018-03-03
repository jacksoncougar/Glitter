import Solver
import Types
import Polyomino
import Board
import Data.Char
import System.Environment
import Terminal

main = do
  putStr title
  putStr description
  args <- getArgs
  let useColor = argColor args
  let useLabels = argLabel args
  board <- getBoard
  let assign = (flip colorize useColor) . (flip tokenize useLabels)
  polyominos <- getPolyominos
  let problem = Problem board $ assign polyominos
  let solution = solve problem
  case solution of
    Nothing -> putStr $ clear ++ "No solution found!"
    Just b  -> do
      putStr $ clear ++ "Found a solution!\n"
      putStr "Looks like this: "
      putStr $ show $ b
  putStr "\nGoodbye.\n"
  return []
  where argColor xs = not $ elem "-nc" xs
        argLabel xs = not $ elem "-nl" xs
        color' :: Int -> Polyomino -> Polyomino
        color' i p@Polyomino{token=t} = p{token = color i t}
        apply as bs = zipWith ($) as bs
  
        tokenize ps True = apply ps polyAlphaTokens 
        tokenize ps _ = apply ps (cycle ["  "])

        colorize ps True = apply (map (flip color') ps) ansi_colors
        colorize ps _ = ps

        
getBoard :: IO Board
getBoard = do
  lines <- getLine
  let words' = (words lines)
  let width = read (words' !! 0) :: Int
  let height = read (words' !! 1) :: Int
  return $  makeBoard width height

polyAlphaTokens = [[x, ' ']  | x <- ['a'..'z']]
polyBlankTokens = cycle ["  "]

title = "                                                \r\n ,----.   ,--.,--.  ,--.    ,--.                \r\n'  .-./   |  |`--',-'  '-.,-'  '-. ,---. ,--.--.\r\n|  | .---.|  |,--.'-.  .-''-.  .-'| .-. :|  .--'\r\n'  '--'  ||  ||  |  |  |    |  |  \\   --.|  |   \r\n `------' `--'`--'  `--'    `--'   `----'`--'   \r\n"

description = "glitter - polyomino board solver\r\n\r\nSYNOPSIS\r\n\tglitter [-nc] [-nl] < file...\r\n\r\nOPTIONS\r\n\t-nc\t: disable ANSI colour output\r\n\r\n\t-nl\t: disable text labels in output\n"

getPolyominos :: IO [Token -> Polyomino]
getPolyominos = do
  lines <- getLine
  let words' = (words lines)
  let count = read (words' !! 0) :: Int
  get count
  where get :: Int -> IO [Token -> Polyomino]
        get 0 = return []
        get x = do
          p <- readPolyomino x
          ps <- get (x-1)
          return (p : ps)

        readPolyomino :: Int -> IO (Token -> Polyomino)
        readPolyomino x = do
          lines <- getLine
          let words' = (words lines)
          let (w:ws) = words'
          let count = read w :: Int
          return $ createPolyomino
            (readPairs count ws)

        readPairs :: Int -> [String] -> [(Int,Int)]
        readPairs 0 _ = []
        readPairs i (a:b:xs) = do
          let x = read a :: Int
          let y = read b :: Int
          (x,y):readPairs (i-1) xs
