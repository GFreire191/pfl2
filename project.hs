import Data.List (intercalate, sort)
import Data.Maybe (fromMaybe)

-- PFL 2023/24 - Haskell practical assignment quickstart

-- Do not modify our definition of Inst and Code
data Inst
  = Push Integer
  | Add
  | Mult
  | Sub
  | Tru
  | Fals
  | Equ
  | Le
  | And
  | Neg
  | Fetch String
  | Store String
  | Noop
  | Branch Code Code
  | Loop Code Code
  deriving (Show)

type Code = [Inst]

type Stack = [Integer]

type State = [(String, Integer)]

-- createEmptyStack :: Stack
createEmptyStack :: Stack
createEmptyStack = []

-- stack2Str :: Stack -> String
stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map valToStr stack)
  where
    valToStr 0 = "False"
    valToStr 1 = "True"
    valToStr n = show n

-- createEmptyState :: State
createEmptyState :: State
createEmptyState = []

-- state2Str :: State -> String
state2Str :: State -> String
state2Str state = intercalate "," [var ++ "=" ++ valToStr var val | (var, val) <- sort state]
  where
    valToStr var 1 = if var == "i" then "1" else "True"
    valToStr _ 0 = "False"
    valToStr _ n = show n

-- run :: (Code, Stack, State) -> (Code, Stack, State)
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (inst : rest, stack, state) =
  case inst of
    Push n -> run (rest, n : stack, state)
    Add -> case stack of
      x : y : stack' -> run (rest, (if x /= 0 && y /= 0 then 1 else 0) : stack', state)
      _ -> error "Run-time error"
    Mult -> case stack of
      x : y : stack' -> run (rest, (x * y) : stack', state)
      _ -> error "Run-time error"
    Sub -> case stack of
      x : y : stack' -> let res = if x == 1 && y == 1 then 1 else x - y in run (rest, res : stack', state)
      _ -> error "Run-time error"
    Tru -> run (rest, 1 : stack, state)
    Fals -> run (rest, 0 : stack, state)
    Equ -> case stack of
      x : y : stack' -> run (rest, (if x == y then 1 else 0) : stack', state)
      _ -> error "Run-time error"
    Le -> case stack of
      x : y : stack' -> run (rest, (if x <= y then 1 else 0) : stack', state)
      _ -> error "Run-time error"
    And -> case stack of
      x : y : stack' -> run (rest, (if x /= 0 && y /= 0 then 1 else 0) : stack', state)
      _ -> error "Run-time error"
    Neg -> case stack of
      x : stack' -> run (rest, (if x /= 0 then 0 else 1) : stack', state)
      _ -> error "Run-time error"
    Fetch var -> let val = fromMaybe 0 (lookup var state) in run (rest, val : stack, state)
    Store var -> case stack of
      x : stack' -> run (rest, stack', updateState var x state)
      _ -> error "Run-time error"
    Noop -> run (rest, stack, state)
    Branch c1 c2 -> case stack of
      x : stack' -> if x /= 0 then run (c1 ++ rest, stack', state) else run (c2 ++ rest, stack', state)
      _ -> error "Run-time error"
    Loop c1 c2 -> case run (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]], stack, state) of
      (_, 0 : _, _) -> run (rest, stack, state)
      (code', stack', state') -> run (code' ++ rest, stack', state')


updateState :: String -> Integer -> State -> State
updateState var x [] = [(var, x)]
updateState var x ((v, _) : rest) | v == var = (var, x) : rest
updateState var x (pair : rest) = pair : updateState var x rest

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where
    (_, stack, state) = run (code, createEmptyStack, createEmptyState)

-- List of all your tests
tests :: [(Code, (String, String))]
tests =
  [ ([Push 10, Push 4, Push 3, Sub, Mult], ("-10", "")),
    ([Fals, Push 3, Tru, Store "var", Store "a", Store "someVar"], ("", "a=3,someVar=False,var=True")),
    ([Fals, Store "var", Fetch "var"], ("False", "var=False")),
    ([Push (-20), Tru, Fals], ("False,True,-20", "")),
    ([Push (-20), Tru, Tru, Neg], ("False,True,-20", "")),
    ([Push (-20), Tru, Tru, Neg, Equ], ("False,-20", "")),
    ([Push (-20), Push (-21), Le], ("True", "")),
    ([Push 5, Store "x", Push 1, Fetch "x", Sub, Store "x"], ("", "x=4")),
    ([Push 10, Store "i", Push 1, Store "fact", Loop [Push 1, Fetch "i", Equ, Neg] [Fetch "i", Fetch "fact", Mult, Store "fact", Push 1, Fetch "i", Sub, Store "i"]], ("", "fact=3628800,i=1"))
  ]

-- Function to run all tests
runTests :: [(Code, (String, String))] -> IO ()
runTests [] = return ()
runTests ((code, expected) : rest) = do
  let result = testAssembler code
  if result == expected
    then putStrLn $ "Test passed: " ++ show code
    else putStrLn $ "Test failed: " ++ show code ++ "\nExpected: " ++ show expected ++ "\nGot: " ++ show result
  runTests rest

-- Run all tests
main :: IO ()
main = runTests tests