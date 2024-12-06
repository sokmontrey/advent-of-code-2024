import Data.Function

main = do
  inp_str <- readFile "input.txt"

  let reports = lines inp_str & map parseLevel
  let count = reports & filter canBeSafe & length

  print count

parseLevel :: String -> [Int]
parseLevel report = map read (words report)

isSafe :: [Int] -> Bool
isSafe levels =
  let diffs = zipWith (-) (tail levels) levels
   in all (\x -> abs x > 0 && abs x < 4) diffs
        && (all (> 0) diffs || all (< 0) diffs)

canBeSafe :: [Int] -> Bool
canBeSafe levels
  | isSafe levels = True
  | otherwise =
      let n = length levels
          all_i = [0 .. n - 1]
      in any (\i -> isSafe (removeAt i levels)) all_i

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i + 1) xs
