module Logic where

import Data

-- Compares two stack elements for equality
equal :: StackElement -> StackElement -> BoolElement
equal (Integer a) (Integer b) 
    | a == b = TT 
    | otherwise = FF
equal (BoolElement TT) (BoolElement FF) = FF
equal (BoolElement FF) (BoolElement TT) = FF
equal (BoolElement TT) (BoolElement TT) = TT
equal (BoolElement FF) (BoolElement FF) = TT

-- Compares two integers for less than or equal
le :: Integer -> Integer -> BoolElement
le a  b
    | a<=b = TT
    | otherwise = FF

-- Performs logical AND operation on two bool elements
logicaland:: BoolElement -> BoolElement -> BoolElement
logicaland  TT  FF = FF
logicaland  FF  TT = FF
logicaland  TT  TT = TT
logicaland  FF  FF = FF

-- Performs logical negation on a bool element
logicalneg::BoolElement -> BoolElement
logicalneg TT = FF
logicalneg FF = TT