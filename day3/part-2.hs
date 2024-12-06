{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

import Data.Function
import Data.Maybe (fromJust, fromMaybe)
import Distribution.Fields (ParseResult)
import Distribution.Simple.Program.HcPkg (list)

type Parser a = String -> Maybe (a, String)

char :: Char -> Parser Char
char _ [] = Nothing
char c (x : rest)
  | c == x = Just (c, rest)
  | otherwise = Nothing

digit :: Parser Char
digit [] = Nothing
digit (x : rest)
  | x `elem` ['0' .. '9'] = Just (x, rest)
  | otherwise = Nothing

string :: String -> Parser String
string "" input = Just ("", input)
string (x : xs) input = case char x input of
  Nothing -> Nothing
  Just (match, rest) -> case string xs rest of
    Nothing -> Nothing
    Just (matches, final) -> Just (match : matches, final)

number :: Parser String
number [] = Just ("", [])
number input = case digit input of
  Nothing -> Nothing
  Just (match, rest) -> case number rest of
    Nothing -> Just ([match], rest)
    Just (matches, final) -> Just (match : matches, final)

every :: [Parser a] -> Parser [a]
every _ [] = Nothing
every [] input = Just ([], input)
every (p : ps) input = case p input of
  Nothing -> Nothing
  Just (match, rest) -> case every ps rest of
    Just (matches, final) -> Just (match : matches, final)
    Nothing -> Nothing

untilEmpty :: Parser a -> Parser [a]
untilEmpty _ [] = Just ([], [])
untilEmpty p input = case p input of
  Nothing -> case input of
    [] -> Just ([], [])
    (_ : rest) -> untilEmpty p rest
  Just (match, rest) -> case untilEmpty p rest of
    Just (matches, final) -> Just (match : matches, final)
    Nothing -> Just ([match], rest)

dontParser = string "don't()"

doParser = string "do()"

mulParser = every [string "mul(", number, string ",", number, string ")"]

mainParser :: Bool -> Parser a -> Parser [a]
mainParser _ _ [] = Just ([], [])
mainParser False p input = case doParser input of
  Nothing -> mainParser False p (tail input)
  Just (_, rest) -> mainParser True p rest
mainParser True p input
  | Just (_, rest) <- dontParser input = mainParser False p rest
  | otherwise = case p input of
    Nothing -> case input of
      [] -> Just ([], [])
      (_ : rest) -> mainParser True p rest
    Just (match, rest) -> case mainParser True p rest of
      Just (matches, final) -> Just (match : matches, final)
      Nothing -> Just ([match], rest)

main = do
  input_str <- readFile "input.txt"

  let maybe_extracted = mainParser True mulParser (input_str ++ "%")
  let parsed = fromJust maybe_extracted & fst & filter (not . null)
  let numbers1 = map (\x -> read (x!!1) :: Int) parsed 
  let numbers2 = map (\x -> read (x!!3) :: Int) parsed 
  print $ zipWith (*) numbers2 numbers1 & sum
