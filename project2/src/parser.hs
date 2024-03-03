module Parser where

import Data
import Utils

-- Parses a string into a program
parse :: String -> Program
parse input = do
    let(result) =lexer input
    parseProgram (result)

-- Parses a list of tokens into a program
parseProgram :: [String] -> Program
parseProgram [] = []
parseProgram (x:":=":xs) = let(statement, rest) = parseAssignStm (x:":=":xs) in statement : parseProgram rest
parseProgram (x:xs)
    | x == "if" = let(statement, rest) = parseIfStm xs in statement : parseProgram rest
    | x == "while" = let(statement, rest) = parseWhileStm xs in statement : parseProgram rest
    | x == ";" = parseProgram xs
    | x == "(" = 
        let(sequence, rest) = splitAtBalanced ";" (x:xs)
        in parseSeq sequence : parseProgram rest

-- Parses a sequence statement from a list of tokens
parseSeq :: [String] -> Stm
parseSeq expr@(x:xs) = parseSeqWithoutParentheses (removeOuterParentheses expr) 
    where
        parseSeqWithoutParentheses :: [String] -> Stm
        parseSeqWithoutParentheses expr@(x:":=":xs) =
            let(statement, rest) = parseAssignStm (x:":=":xs) 
                in case rest of
                    [] -> statement
                    ";":[] -> statement
                    _ -> Seq statement (parseSeq rest)
        parseSeqWithoutParentheses expr@(x:xs)
            | x == "if" = 
                let(statement, rest) = parseIfStm xs
                in case rest of
                    [] -> statement
                    ";":[] -> statement
                    _ -> Seq statement (parseSeq rest)
            | x == "while" = 
                let(statement, rest) = parseWhileStm xs
                in case rest of
                    [] -> statement
                    ";":[] -> statement
                    _ -> Seq statement (parseSeq rest)
            | x == ";" = parseSeq xs
            | x == "(" =
                let(statement, rest) = splitAtBalanced ";" expr
                in case rest of
                    [] -> parseSeq statement
                    ";":[] -> parseSeq statement
                    _ -> Seq (parseSeq statement) (parseSeq rest)

-- Parses an if statement from a list of tokens
parseIfStm :: [String] -> (Stm, [String])
parseIfStm expr@(x:xs) =
    let (expression1, rest1) = splitAtBalanced "then" expr
        (expression2, rest2) = splitAtBalanced "else" rest1
        (expression3, rest3) = splitAtBalanced ";" rest2
        statement1 = parseSeq(expression2)
        statement2 = parseSeq(expression3)
    in (If (parseBexp expression1) statement1 statement2, rest3)

-- Parses a while statement from a list of tokens
parseWhileStm :: [String] -> (Stm, [String])
parseWhileStm expr@(x:xs) =
    let (expression1, rest1) = splitAtBalanced "do" expr
        (expression2, rest2) = splitAtBalanced ";" rest1 
        statement1 = parseSeq(expression2) 
    in (While (parseBexp expression1) statement1, rest2) 

-- Parses an assignment statement from a list of tokens
parseAssignStm :: [String] -> (Stm, [String])
parseAssignStm (x:":=":xs) =
    let (expression, rest) = span (/= ";") xs in (AssignA x (parseAexp expression), rest)

-- Parses an arithmetic expression from a list of tokens
parseAexp :: [String] -> Aexp
parseAexp expr@(x:xs) = parseAexpWithoutParentheses (removeOuterParentheses expr)
    where 
        parseAexpWithoutParentheses :: [String] -> Aexp
        parseAexpWithoutParentheses (x:[])
            | isInteger x = Num (read x)
            | otherwise = VarA x
        parseAexpWithoutParentheses expr@(x:xs)
            | "+" `elem` expr, let (left, a:rest) = span (/= "+") expr, parenthesesBalanced left
            = Add' (parseAexp left) (parseAexp rest)
            | "-" `elem` expr, let (left, a:rest) = span (/= "-") expr, parenthesesBalanced left
            = Sub' (parseAexp left) (parseAexp rest)
            | "*" `elem` expr, let (left, a:rest) = span (/= "*") expr, parenthesesBalanced left
            = Mult' (parseAexp left) (parseAexp rest)

-- Parses a boolean expression from a list of tokens
parseBexp :: [String] -> Bexp
parseBexp expr@(x:xs) = parseBexpWithoutParentheses (removeOuterParentheses expr)
    where
        parseBexpWithoutParentheses :: [String] -> Bexp
        parseBexpWithoutParentheses (x:[])
            | x == "True" = BoolValue True
            | x == "False" = BoolValue False
        parseBexpWithoutParentheses expr@(x:xs)
            | "and" `elem` expr, let (left, a:rest) = span (/= "and") expr, parenthesesBalanced left
            = And' (parseBexp left) (parseBexp rest)
            | "=" `elem` expr, let (left, a:rest) = span (/= "=") expr, parenthesesBalanced left
            = EqB (parseBexp left) (parseBexp rest)
            | x == "not"
            = Not' (parseBexp xs)
            | "==" `elem` expr, let (left, a:rest) = span (/= "==") expr, parenthesesBalanced left
            = EqA (parseAexp left) (parseAexp rest)
            | "<=" `elem` expr, let (left, a:rest) = span (/= "<=") expr, parenthesesBalanced left
            = Le' (parseAexp left) (parseAexp rest)