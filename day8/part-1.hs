{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use map once" #-}
{-# HLINT ignore "Use concatMap" #-}

import Data.Function
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set

type Grid = [[Char]]

printGrid :: Grid -> IO ()
printGrid = mapM_ putStrLn

notDot :: Grid -> [(Int, Int, Char)]
notDot grid =
  [ (i, j, cell)
    | (i, col) <- zip [0 ..] grid,
      (j, cell) <- zip [0 ..] col,
      cell /= '.'
  ]

coord :: (Int, Int, Char) -> (Int, Int)
coord (a, b, _) = (a, b)

thrd :: (Int, Int, Char) -> Char
thrd (_, _, c) = c

type Vec2 = (Int, Int)

type Dim = Vec2

type Pos = Vec2

makePairs :: [a] -> [(a, a)]
makePairs [] = []
makePairs (ele : xs) = zip [ele | _ <- xs] xs ++ makePairs xs

isInBound :: Pos -> Dim -> Bool
isInBound (r, c) (rows, cols) = r >= 0 && c >= 0 && r < rows && c < cols

placeAntinode :: Pos -> Dim -> Set.Set Pos -> Set.Set Pos
placeAntinode pos dim list =
  if isInBound pos dim
    then Set.insert pos list
    else list

placeAntinodePair :: Dim -> Set.Set Pos -> (Pos, Pos) -> Set.Set Pos
placeAntinodePair dim list (a, b) =
  list
    & placeAntinode a dim
    & placeAntinode b dim

getAntinodes :: (Pos, Pos) -> (Pos, Pos)
getAntinodes ((ar, ac), (br, bc)) =
  let (abr, abc) = (br - ar, bc - ac)
   in ((br + abr, bc + abc), (ar - abr, ac - abc))

main = do
  input_str <- readFile "input.txt"

  let grid = lines input_str
  let rows = length grid
  let cols = length (head grid)

  let antinodes =
        grid
          & notDot
          & List.sortOn thrd
          & List.groupBy (\a b -> thrd a == thrd b)
          & map (map (\(a, b, c) -> (a, b)))
          & map makePairs
          & concat
          & map getAntinodes
          & foldl (placeAntinodePair (rows, cols)) Set.empty

  print $ antinodes & length
