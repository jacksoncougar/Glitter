import Solver
import Polyomino
import Types
import Board

test = Problem board' polyominos'

board' = makeBoard 7 4

polyominos' = [
  createPolyomino [(0,0), (0,1), (1,1), (2,1), (2,0), (3,0)] 'a'
  , createPolyomino [(0,0), (0,1), (1,1), (2,1), (3,1), (3,0)] 'b'
  , createPolyomino [(0,0)] 'c'
  , createPolyomino [(0,0), (0,1), (1,1), (1,2), (1,3)] 'd'
  , createPolyomino [(0,1), (1,0), (1,1), (2,0), (3,0)] 'e'
  , createPolyomino [(0,0), (1,0), (1,1), (1,2), (2,2)] 'f'
  ]
