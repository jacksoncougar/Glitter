import Solver
import Polyomino
import Board
import Data.Char

main = do
  board <- getBoard
  polyominos <- getPolyominos
  let problem = Problem board polyominos
  let solution = solve''' problem
  case solution of
    Nothing -> putStr "No solution found!"
    Just b  -> do
      putStr "Found a solution!\n"
      putStr "Looks like this: "
      putStr $ show $ b
  putStr "\nGoodbye.\n"
  return []

getBoard :: IO Board
getBoard = do
  lines <- getLine
  let words' = (words lines)
  let width = read (words' !! 0) :: Int
  let height = read (words' !! 1) :: Int
  return $  makeBoard width height

getPolyominos :: IO [Polyomino]
getPolyominos = do
  lines <- getLine
  let words' = (words lines)
  let count = read (words' !! 0) :: Int
  get count
  where get :: Int -> IO [Polyomino]
        get 0 = return []
        get x = do
          p <- readPolyomino x
          ps <- get (x-1)
          return (p : ps)

        readPolyomino :: Int -> IO Polyomino
        readPolyomino x = do
          lines <- getLine
          let words' = (words lines)
          let (w:ws) = words'
          let count = read w :: Int
          return $ createPolyomino (readPairs count ws) (chr (x + 65))

        readPairs :: Int -> [String] -> [(Int,Int)]
        readPairs 0 _ = []
        readPairs i (a:b:xs) = do
          let x = read a :: Int
          let y = read b :: Int
          (x,y):readPairs (i-1) xs
