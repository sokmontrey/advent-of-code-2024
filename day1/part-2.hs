import Data.Function
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

main = do
  inp_str <- readFile "input.txt"

  let (lefts, rights) =
        inp_str
          & lines
          & map splitNum
          & unzip

  let occurences =
        rights
          & List.sort
          & List.group
          & map (\x -> (head x, length x))

  let occurences_map =
        Map.fromList occurences

  let result =
        lefts
          & map (fromMaybe 0 . (`Map.lookup` occurences_map))
          & zipWith (*) lefts
          & sum

  print result

splitNum :: String -> (Int, Int)
splitNum line = (read x, read y)
  where
    [x, y] = words line
