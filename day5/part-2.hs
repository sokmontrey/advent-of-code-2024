{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use map once" #-}

import Data.Char (isAlpha)
import Data.Function
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Distribution.Simple.PackageIndex (dependencyGraph)

splitComma :: String -> [String]
splitComma [] = []
splitComma str =
  let (firstPart, rest) = break (== ',') str
   in if null rest
        then [firstPart]
        else firstPart : splitComma (tail rest)

parentNotContains :: Set.Set Int -> [Int] -> Bool
parentNotContains parents pages = pages & any (`Set.member` parents) & not

isInOrder :: Map.Map Int (Set.Set Int) -> [Int] -> Bool
isInOrder parent_table = isInOrder'
  where
    isInOrder' [] = True
    isInOrder' (page : rest) = case Map.lookup page parent_table of
      Nothing -> isInOrder parent_table rest
      Just parents -> parents `parentNotContains` rest && isInOrder parent_table rest

getMiddle :: [Int] -> Int
getMiddle pages = pages !! (length pages `div` 2)

compareByParent :: Map.Map Int (Set.Set Int) -> Int -> Int -> Ordering
compareByParent parent_table a b = case Map.lookup a parent_table of
  Nothing -> EQ
  Just parents -> if Set.member b parents then GT else LT

sortByParentTable :: Map.Map Int (Set.Set Int) -> [Int] -> [Int]
sortByParentTable parent_table = List.sortBy (compareByParent parent_table)

main = do
  input_str <- readFile "input.txt"

  let total_lines = lines input_str
  let (raw_edges, _ : raw_pages) = break (== []) total_lines

  let edges =
        raw_edges
          & map (break (== '|'))
          & map (\(a, _ : b) -> (a, b))
          & map (\(a, b) -> (read a :: Int, read b :: Int))

  let grouped_edges =
        edges
          & map (\(a, b) -> (b, a))
          & List.sortOn fst
          & List.groupBy (\a b -> fst a == fst b)

  let parent_sets =
        grouped_edges
          & map (map snd)
          & map Set.fromList

  let parent_table =
        parent_sets
          & zip (map (fst . head) grouped_edges)
          & Map.fromList

  let pages =
        raw_pages
          & map (\raw_page -> splitComma raw_page & map (\x -> read x :: Int))

  let in_correct_pages =
        pages
          & filter (not . isInOrder parent_table)
          & map (sortByParentTable parent_table)

  print $
    in_correct_pages
      & map getMiddle
      & sum
