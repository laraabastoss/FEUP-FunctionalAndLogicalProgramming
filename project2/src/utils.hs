module Utils where

import Data
import Data.List (isInfixOf)
import Data.Char

-- Checks if a given string represents an integer
isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
    [(n, "")] -> True
    _         -> False

-- Checks if there are the same number of opened and closed parentheses in a list of tokens
parenthesesBalanced :: [String] -> Bool
parenthesesBalanced str = go str 0
  where
    go :: [String] -> Int -> Bool
    go [] count = count == 0
    go (c:cs) count
      | c == "("  = go cs (count + 1)
      | c == ")"  = count > 0 && go cs (count - 1)
      | otherwise = go cs count

-- Removes outer parentheses from a list of tokens
removeOuterParentheses :: [String] -> [String]
removeOuterParentheses expr
  | hasOuterParentheses expr = init (tail expr)
  | otherwise = expr
  where
    hasOuterParentheses :: [String] -> Bool
    hasOuterParentheses [] = False
    hasOuterParentheses ["("] = False
    hasOuterParentheses expr =
      let headExpr = head expr
          lastExpr = last expr
      in headExpr == "(" && lastExpr == ")" && parenthesesBalanced (init (tail expr))

-- Splits a list of tokens at the first occurrence of a target token with balanced parentheses
splitAtBalanced :: String -> [String] -> ([String], [String])
splitAtBalanced target strs = go strs 0 []
  where
    go :: [String] -> Int -> [String] -> ([String], [String])
    go [] _ acc = (reverse acc, [])
    go (c:cs) count acc
      | c == "("  = go cs (count + 1) (c : acc)
      | c == ")"  = if count > 0
                    then go cs (count - 1) (c : acc)
                    else go cs count (c : acc)
      | isInfixOf target c && count == 0 = (reverse acc, cs)
      | otherwise = go cs count (c : acc)

-- Splits the input string into a list of tokens
lexer :: String -> [String]
lexer [] = []
lexer str@(c:cs)
  | isSpace c = lexer (dropWhile isSpace cs) 
  | isAlpha c = let (word, rest) = span isAlphaNum str in word : lexer rest
  | isDigit c = let (num, rest) = span isDigit str in num : lexer rest
  | c == '=' && head cs == '=' = "==" : lexer (drop 1 cs)
  | c == ':' && head cs == '=' = ":=" : lexer (drop 1 cs)
  | c == '<' && head cs == '=' = "<=" : lexer (drop 1 cs)
  | otherwise = [c] : lexer cs