import Data.List (intercalate, sort, deleteBy)
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

type Stack = [(Integer, Bool)]

type State = [(String, (Integer, Bool))]

createEmptyStack :: Stack
createEmptyStack = []

stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map valToStr stack)
  where
    valToStr (1, True) = "True"
    valToStr (0, _) = "False"
    valToStr (n, _) = show n

createEmptyState :: State
createEmptyState = []

state2Str :: State -> String
state2Str state = intercalate "," [var ++ "=" ++ valToStr val | (var, val) <- sort state]
  where
    valToStr (1, True) = "True"
    valToStr (0, _) = "False"
    valToStr (n, _) = show n

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (inst : rest, stack, state) =
  case inst of
    Push n -> run (rest, (n, False) : stack, state)
    Add -> case stack of
      (x, _) : (y, _) : stack -> run (rest, (x + y, False) : stack, state)
      _ -> error "Run-time error"
    Mult -> case stack of
      (x, _) : (y, _) : stack -> run (rest, (x * y, False) : stack, state)
      _ -> error "Run-time error"
    Sub -> case stack of
      (x, _) : (y, _) : stack -> run (rest, (x - y, False) : stack, state)
      _ -> error "Run-time error"
    Tru -> run (rest, (1, True) : stack, state)
    Fals -> run (rest, (0, True) : stack, state)
    Equ -> case stack of
      (x, _) : (y, _) : stack -> run (rest, (if x == y then (1, True) else (0, True)) : stack, state)
      _ -> error "Run-time error"
    Le -> case stack of
      (x, _) : (y, _) : stack -> run (rest, (if x <= y then (1, True) else (0, True)) : stack, state)
      _ -> error "Run-time error"
    And -> case stack of
      (x, _) : (y, _) : stack -> 
        if (x == 0 || x == 1) && (y == 0 || y == 1) then
          run (rest, (if x /= 0 && y /= 0 then (1, True) else (0, True)) : stack, state)
        else
          error "Run-time error"
    Neg -> case stack of
      (x, _) : stack -> run (rest, (if x /= 0 then (0, True) else (1, True)) : stack, state)
      _ -> error "Run-time error"
    Fetch var -> let val = fromMaybe (0, False) (lookup var state) in run (rest, val : stack, state)
    Store var -> case stack of
      (x, isBool) : stack -> run (rest, stack, updateState var (x, isBool) state)
      _ -> error "Run-time error"
    Noop -> run (rest, stack, state)
    Branch c1 c2 -> case stack of
      (x, _) : stack -> if x /= 0 then run (c1 ++ rest, stack, state) else run (c2 ++ rest, stack, state)
      _ -> error "Run-time error"
    Loop c1 c2 -> case run (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]], stack, state) of
      (_, (0, _) : _, _) -> run (rest, stack, state)
      (code', stack, state') -> run (code' ++ rest, stack, state')

updateState :: String -> (Integer, Bool) -> State -> State
updateState var val state = (var, val) : deleteBy (\(var1, _) (var2, _) -> var1 == var2) (var, val) state



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
    ([Push 10, Store "i", Push 1, Store "fact", Loop [Push 1, Fetch "i", Equ, Neg] [Fetch "i", Fetch "fact", Mult, Store "fact", Push 1, Fetch "i", Sub, Store "i"]], ("", "fact=3628800,i=1")),
    ([Push 10, Store "n", Push 0, Store "a", Push 1, Store "b", Loop [Push 1, Fetch "n", Equ, Neg] [Fetch "a", Fetch "b", Add, Store "temp", Fetch "b", Store "a", Fetch "temp", Store "b", Push 1, Fetch "n", Sub, Store "n"]], ("", "a=34,b=55,n=1,temp=55")),
    ([ Push 10, Store "n", Push 1, Store "result", Loop [ Push 1, Fetch "n", Equ, Neg ] 
      [ Fetch "n", Fetch "result", Mult, Store "result", Push 1, Fetch "n", Sub, Store "n" ]], ("", "n=1,result=3628800"))
      
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