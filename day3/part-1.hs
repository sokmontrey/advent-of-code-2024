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

mulParser = every [string "mul(", number, string ",", number, string ")"]

main = do
  input_str <- readFile "input.txt"

  let maybe_extracted = untilEmpty mulParser (input_str ++ "%")
  let parsed = fromJust maybe_extracted & fst & filter (not . null)
  let numbers1 = map (\x -> read (x!!1) :: Int) parsed
  let numbers2 = map (\x -> read (x!!3) :: Int) parsed

  print $ zipWith (*) numbers2 numbers1 & sum
