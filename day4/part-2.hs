import Data.Function
import Language.Haskell.TH (fromE)

main = do
  input_str <- readFile "input.txt"
  let grid = lines input_str

  print $
    map (fromEnum . isXMAS grid) (grid
      & findCoords 'A') & sum

isXMAS :: [[Char]] -> Vec2 -> Bool
isXMAS grid pos =
  let (q1, q2, q3, q4) = getQs grid pos
   in ((q1 == 'M' && q4 == 'S') || (q1 == 'S' && q4 == 'M'))
        && ((q2 == 'M' && q3 == 'S') || (q2 == 'S' && q3 == 'M'))

getQs :: [[Char]] -> Vec2 -> (Char, Char, Char, Char)
getQs grid pos = (getQ1 grid pos, getQ2 grid pos, getQ3 grid pos, getQ4 grid pos)

getQ1 :: [[Char]] -> Vec2 -> Char
getQ1 grid (r, c) = getValue grid (r - 1, c - 1)

getQ2 :: [[Char]] -> Vec2 -> Char
getQ2 grid (r, c) = getValue grid (r - 1, c + 1)

getQ3 :: [[Char]] -> Vec2 -> Char
getQ3 grid (r, c) = getValue grid (r + 1, c - 1)

getQ4 :: [[Char]] -> Vec2 -> Char
getQ4 grid (r, c) = getValue grid (r + 1, c + 1)

type Vec2 = (Int, Int)

type Dir = [Vec2]

other = '#'

getValue :: [String] -> (Int, Int) -> Char
getValue grid (r, c)
  | c < 0 || r < 0 = other
  | r >= length grid = other
  | c >= length (head grid) = other
  | otherwise = (grid !! r) !! c

findCoords :: Char -> [String] -> [(Int, Int)]
findCoords c grid =
  [ (row_i, col_i)
    | (row_i, row) <- zip [0 ..] grid,
      (col_i, cell) <- zip [0 ..] row,
      cell == c
  ]
