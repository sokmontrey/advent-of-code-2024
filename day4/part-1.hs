import Data.Function

main = do
  input_str <- readFile "input.txt"
  let grid = lines input_str

  print $
    grid
      & findXCoords
      & map (countXMAS grid)
      & sum

type Vec2 = (Int, Int)

type Dir = [Vec2]

other = '#'

getValue :: [String] -> (Int, Int) -> Char
getValue grid (r, c)
  | c < 0 || r < 0 = other
  | r >= length grid = other
  | c >= length (head grid) = other
  | otherwise = (grid !! r) !! c

findXCoords :: [String] -> [(Int, Int)]
findXCoords grid =
  [ (row_i, col_i)
    | (row_i, row) <- zip [0 ..] grid,
      (col_i, cell) <- zip [0 ..] row,
      cell == 'X'
  ]

chars = ['M', 'A', 'S']

dirs =
  [ [(0, 1), (0, 2), (0, 3)], -- right
    [(0, -1), (0, -2), (0, -3)], -- left
    [(1, 0), (2, 0), (3, 0)], -- down
    [(-1, 0), (-2, 0), (-3, 0)], -- up
    [(-1, -1), (-2, -2), (-3, -3)], -- up left
    [(1, 1), (2, 2), (3, 3)], -- down right
    [(-1, 1), (-2, 2), (-3, 3)], -- up right
    [(1, -1), (2, -2), (3, -3)] -- down left
  ]

-- and [let (rd, cd) = (dir !! i) in (chars !! i) == getValue grid (rd + r, cd + c) | i <- [0 .. 2]]
processDir :: [String] -> Vec2 -> Dir -> Bool
processDir grid (r, c) dir =
  dir
    & map (\(rd, cd) -> getValue grid (rd + r, c + cd))
    & zipWith (==) [chars !! i | i <- [0 .. 2]]
    & and

countXMAS :: [String] -> Vec2 -> Int
countXMAS grid pos =
  dirs
    & map (fromEnum . processDir grid pos)
    & sum
