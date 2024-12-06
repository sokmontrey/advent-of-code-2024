import Data.Function

main = do
  inp_str <- readFile "input.txt"

  let reports = lines inp_str & map parseLevel
  let diffs = map getDiff reports
  let is_same_signs = map (\diff -> isAllPositive diff || isAllNegative diff) diffs
  let is_in_range = map (not . isOutOfRange) diffs
  let is_safe = zipWith (&&) is_same_signs is_in_range
  let count = sum $ map fromEnum is_safe

  print count

parseLevel :: String -> [Int]
parseLevel report = map read (words report)

getDiff :: [Int] -> [Int]
getDiff levels = zipWith (-) (tail levels) levels

isOutOfRange :: [Int] -> Bool
isOutOfRange = any (\x -> abs x < 1 || abs x > 3)

isAllPositive :: [Int] -> Bool
isAllPositive diffs = all (\x -> x > 0 && head diffs > 0) diffs

isAllNegative :: [Int] -> Bool
isAllNegative diffs = all (\x -> x < 0 && head diffs < 0) diffs
