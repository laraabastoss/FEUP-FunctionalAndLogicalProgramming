import Data
import Display
import Parser
import Data.Char (isDigit, digitToInt)
import Logic

-- Runs the stack machine with a given initial state
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (Tru :code, stack, state) = run (code, BoolElement TT:stack, state)
run (Fals :code, stack, state) = run (code, BoolElement FF:stack, state)
run (Add :code, (Integer x1 : Integer x2 : xs), state) = run (code,  Integer ( x1 +  x2 ) : xs, state)
run (Mult :code, (Integer x1 : Integer x2 : xs), state) = run (code,  Integer ( x1 *  x2 ) : xs, state)
run (Sub :code, (Integer x1 : Integer x2 : xs), state) = run (code,  Integer ( x1 -  x2 ) : xs, state)
run (Equ :code, ( x1 :  x2 : xs), state) = run (code,  BoolElement (equal x1 x2)  : xs, state)
run (Le :code, ( Integer x1 :  Integer x2 : xs), state) = run (code,  BoolElement (le x1 x2)  : xs, state)
run (And :code, ( BoolElement x1 :  BoolElement x2 : xs), state) = run (code,  BoolElement (logicaland x1 x2)  : xs, state)
run (Neg :code, ( BoolElement x : xs), state) = run (code,  BoolElement (logicalneg x)  : xs, state)
run (Push n :code, stack, state) = run (code, Integer n:stack, state)
run (Fetch var :code, stack, state)
    | Just value <- lookup var state = run (code, value:stack, state)
run (Store var :code, x:xs, state) =
  case lookup var state of
    Just _  -> run (code, xs, updateState var x state)
    Nothing -> run (code, xs, insertInOrder (var, x) state)
  where
    -- Updates the variable in state with a new value
    updateState :: String -> StackElement -> State -> State
    updateState var newVal [] = [(var, newVal)]
    updateState var newVal ((oldVar, oldVal):rest)
      | var == oldVar = (var, newVal):rest
      | otherwise     = (oldVar, oldVal) : updateState var newVal rest
    -- Inserts a new variable-value pair into the state in order
    insertInOrder :: (String, StackElement) -> State -> State
    insertInOrder newVar [] = [newVar]
    insertInOrder newVar (varVal:rest)
      | fst newVar <= fst varVal = newVar : varVal : rest
      | otherwise               = varVal : insertInOrder newVar rest
run (Branch code1 code2 :code, BoolElement TT:xs, state) = run (code1 ++ code, xs, state)
run (Branch code1 code2 :code, BoolElement FF:xs, state) = run (code2 ++ code, xs, state)
run (Noop:code, stack, state) = run (code, stack, state)
run (Loop code1 code2 :code, stack, state) =  run ( code1 ++  (Branch ( code2 ++[ Loop code1 code2])  [Noop] ) :code, stack, state)
run (_, _, _) = error "Run-time error"

-- Compiles an arithmetic expression into code
compA :: Aexp -> Code
compA (Num n) = [Push n]
compA (VarA x) = [Fetch x]
compA (Add' a1 a2) = compA a2 ++ compA a1 ++ [Add]
compA (Mult' a1 a2) = compA a2 ++ compA a1 ++ [Mult]
compA (Sub' a1 a2) = compA a2 ++ compA a1 ++ [Sub]

-- Compiles a boolean expression into code
compB :: Bexp -> Code
compB (BoolValue True) = [Tru]
compB (BoolValue False) = [Fals]
compB (VarB x) = [Fetch x]
compB (EqA a1 a2) = compA a2 ++ compA a1 ++ [Equ]
compB (EqB a1 a2) = compB a2 ++ compB a1 ++ [Equ]
compB (Le' a1 a2) = compA a2 ++ compA a1 ++ [Le]
compB (And' b1 b2) = compB b2 ++ compB b1 ++ [And]
compB (Not' b) = compB b ++ [Neg]

-- Compiles a program into code
compile :: Program -> Code
compile [] = []
compile (Aexp x : rest) = compA x ++ compile rest
compile (Bexp x : rest) = compB x ++ compile rest
compile (AssignA x a : rest) = compA a ++ [Store x] ++ compile rest
compile (AssignB x a : rest) = compB a ++ [Store x] ++ compile rest
compile (Seq s1 s2 : rest) = compile [s1, s2] ++ compile rest
compile (If b s1 s2 : rest) = compB b ++ [Branch (compile [s1]) (compile [s2])] ++ compile rest
compile (While b s : rest) = [Loop (compB b) (compile [s])] ++ compile rest

-- Tests the stack machine with a given code
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Tests the parser with a given program code
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

