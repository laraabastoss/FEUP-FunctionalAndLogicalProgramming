
module Display where

import Data

-- Converts a BoolElement to its string representation
showBoolElement::BoolElement -> String
showBoolElement  TT = "True"
showBoolElement FF = "False"

-- Converts a stack to its string representation
stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str (Integer x : xs) = show x ++ rest
  where
    rest
      | null xs   = ""
      | otherwise = "," ++ stack2Str xs
stack2Str (BoolElement x : xs) = showBoolElement x ++ rest
  where
    rest
      | null xs   = ""
      | otherwise = "," ++ stack2Str xs

-- Converts a state to its string representation
state2Str :: State -> String
state2Str [] = ""
state2Str ((var, Integer val):xs) = var ++ "=" ++ show val ++ rest
  where
    rest
      | null xs   = ""
      | otherwise = "," ++ state2Str xs
state2Str ((var, BoolElement val):xs) = var ++ "=" ++ showBoolElement val ++ rest
  where
    rest
      | null xs   = ""
      | otherwise = "," ++ state2Str xs