module Data where

-- Represents individual instructions
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

-- Represents the code as a sequence of instructions
type Code = [Inst]

-- Represents boolean values
data BoolElement = TT | FF
    deriving (Eq,Show)

-- Represents elements that can be stored in the stack
data StackElement =  Integer Integer |  BoolElement BoolElement
  deriving (Show)

-- Represents the stack as a sequence od stack elements
type Stack = [StackElement]

-- Represents the state of variable bindings
type State = [(String, StackElement)]

-- Creates an empty stack
createEmptyStack :: Stack
createEmptyStack = []

-- Creates an empty state
createEmptyState :: State
createEmptyState = []

-- Represents arithmetic expressions
data Aexp = Add' Aexp Aexp | Mult' Aexp Aexp | Sub' Aexp Aexp | VarA String | Num Integer
  deriving (Show)

-- Represents boolean expressions
data Bexp = EqA Aexp Aexp | EqB Bexp Bexp |Le' Aexp Aexp | Not' Bexp | And' Bexp Bexp | BoolValue Bool | VarB String
  deriving (Show)

-- Represents statements
data Stm = AssignA String Aexp
         | AssignB String Bexp
         | Seq Stm Stm
         | If Bexp Stm Stm
         | While Bexp Stm
         | Aexp Aexp
         | Bexp Bexp
  deriving (Show)

-- Represents a program as a sequence of statements
type Program = [Stm]