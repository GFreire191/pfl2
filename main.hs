import Data.List (deleteBy, intercalate, isPrefixOf, sort)
import Debug.Trace

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
        if (x == 0 || x == 1) && (y == 0 || y == 1)
          then run (rest, (if x /= 0 && y /= 0 then (1, True) else (0, True)) : stack, state)
          else error "Run-time error"
    Neg -> case stack of
      (x, _) : stack -> run (rest, (if x /= 0 then (0, True) else (1, True)) : stack, state)
      _ -> error "Run-time error"
    Fetch var -> case lookup var state of
      Just val -> run (rest, val : stack, state)
      Nothing -> error ("Run-time error: Can not fetch variable '" ++ var ++ "' from state")
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

-- TODO: Define the types Aexp, Bexp, Stm and Program

data Aexp = Num Integer | VarExp String | AddExp Aexp Aexp | SubExp Aexp Aexp | MultExp Aexp Aexp deriving (Show)

data Bexp = EquAExp Aexp Aexp | LeExp Aexp Aexp | AndExp Bexp Bexp | EquBoExp Bexp Bexp | NegExp Bexp | TruB | FalsB deriving (Show)

data Stm = BranchExp Bexp [Stm] [Stm] | LoopExp Bexp [Stm] | Assign String Aexp deriving (Show)

type Program = [Stm]

compA :: Aexp -> Code
compA (Num a) = [Push a]
compA (VarExp a) = [Fetch a]
compA (AddExp a b) = compA b ++ compA a ++ [Add]
compA (SubExp a b) = compA b ++ compA a ++ [Sub]
compA (MultExp a b) = compA b ++ compA a ++ [Mult]

compB :: Bexp -> Code
compB (EquAExp a b) = compA b ++ compA a ++ [Equ]
compB (EquBoExp a b) = compB b ++ compB a ++ [Equ]
compB (LeExp a b) = compA b ++ compA a ++ [Le]
compB (AndExp a b) = compB b ++ compB a ++ [And]
compB (NegExp a) = compB a ++ [Neg]
compB TruB = [Tru]
compB FalsB = [Fals]

compile :: Program -> Code
compile = concatMap compileStm

compileStm :: Stm -> Code
compileStm (Assign var aexp) = compA aexp ++ [Store var]
compileStm (BranchExp bexp stm1 stm2) = compB bexp ++ [Branch (compile stm1) (compile stm2)]
compileStm (LoopExp bexp stm) = [Loop (compB bexp) (compile stm)]




lexer :: String -> [String]
lexer = lexeracc [] ""

lexeracc :: [String] -> String -> String -> [String]
lexeracc acc stracc []
  | stracc == "" = acc
  | otherwise = acc ++ [stracc]
lexeracc acc stracc (c : cs)
  | any (`isPrefixOf` (c : cs)) keywords =
      let (kw, rest) = splitAt (length $ head $ filter (`isPrefixOf` (c : cs)) keywords) (c : cs)
       in if stracc == ""
            then lexeracc (acc ++ [kw]) "" rest
            else lexeracc (acc ++ [stracc, kw]) "" rest
  | c == ' ' =
      if stracc == ""
        then lexeracc acc "" cs
        else lexeracc (acc ++ [stracc]) "" cs
  | otherwise = lexeracc acc (stracc ++ [c]) cs
  where
    keywords = ["while", "if", "then", "else", "*", "+", "/", "-", ";", "(", ")", "<=", "==", "not", "=", "and", ":=", "do"]

parse :: String -> Program
parse str = parseAuxiliary (lexer str) []

parseAuxiliary :: [String] -> [Stm] -> [Stm]
parseAuxiliary [] statements = statements
parseAuxiliary (token : ":=" : remainingTokens) statements =
  let (beforeAssignment, afterAssignment) = break (== ";") (token : ":=" : remainingTokens)
   in case parseExpression (drop 2 beforeAssignment) [("+", AddExp), ("-", SubExp), ("*", MultExp)] of
        Just (expression, []) -> parseAuxiliary (drop 1 afterAssignment) (statements ++ [Assign token expression])
        _ -> error "Parse Error"
parseAuxiliary ("(" : remainingTokens) statements =
  let (beforeClosingParen, afterClosingParen) = break (== ")") ("(" : remainingTokens)
   in parseAuxiliary (drop 1 afterClosingParen) (statements ++ parseAuxiliary (drop 1 beforeClosingParen) [])
parseAuxiliary (";" : remainingTokens) statements = parseAuxiliary remainingTokens statements
parseAuxiliary ("if" : remainingTokens) statements =
  let (beforeThen, afterThen) = break (== "then") ("if" : remainingTokens)
      (beforeElse, afterElse) = break (== "else") afterThen
      tokensAfterElse = drop 1 afterElse
   in case parseAndExpression (checkIfParenthesis (drop 1 beforeThen)) of
        Just (condition, _) -> 
          case takeFirstElement tokensAfterElse of
            "(" ->
              let (beforeClosingParen, afterClosingParen) = break (== ")") tokensAfterElse
               in parseAuxiliary (drop 1 afterClosingParen) (statements ++ [BranchExp condition (parseAuxiliary (drop 1 beforeElse) []) (parseAuxiliary beforeClosingParen [])])
            _ ->
              let (beforeSemicolon, afterSemicolon) = break (== ";") tokensAfterElse
               in parseAuxiliary (drop 1 afterSemicolon) (statements ++ [BranchExp condition (parseAuxiliary (drop 1 beforeElse) []) (parseAuxiliary beforeSemicolon [])])
        _ -> error "Parse Error"
parseAuxiliary ("while" : remainingTokens) statements =
  let (beforeDo, afterDo) = break (== "do") ("while" : remainingTokens)
      tokensAfterDo = drop 1 afterDo
   in case parseAndExpression (checkIfParenthesis (drop 1 beforeDo)) of
        Just (condition, _) -> 
          case takeFirstElement tokensAfterDo of
            "(" ->
              let (beforeClosingParen, afterClosingParen) = break (== ")") tokensAfterDo
               in parseAuxiliary (drop 1 afterClosingParen) (statements ++ [LoopExp condition (parseAuxiliary beforeClosingParen [])])
            _ ->
              let (beforeSemicolon, afterSemicolon) = break (== ";") tokensAfterDo
               in parseAuxiliary (drop 1 afterSemicolon) (statements ++ [LoopExp condition (parseAuxiliary beforeSemicolon [])])
        _ -> error "Parse Error"


        

checkIfParenthesis :: [String] -> [String]
checkIfParenthesis ("(" : remainingTokens) = remainingTokens
checkIfParenthesis remainingTokens = remainingTokens

takeFirstElement :: [String] -> String
takeFirstElement (firstElement : _) = firstElement


----- Aexp ----
parseExpression :: [String] -> [(String, Aexp -> Aexp -> Aexp)] -> Maybe (Aexp, [String])
parseExpression tokens [] = parseInt tokens
parseExpression tokens ((operator, constructor) : rest) = do
  (firstExpression, remainingTokens) <- parseExpression tokens rest
  if null remainingTokens || head remainingTokens /= operator
    then return (firstExpression, remainingTokens)
    else do
      (secondExpression, remainingTokensAfterSecondExpression) <- parseExpression (tail remainingTokens) rest
      return (constructor firstExpression secondExpression, remainingTokensAfterSecondExpression)

parseInt :: [String] -> Maybe (Aexp, [String])
parseInt ("(" : rest) = do
  (expr, restString1) <- parseExpression rest [("+", AddExp), ("-", SubExp), ("*", MultExp)]
  if head restString1 == ")"
    then return (expr, tail restString1)
    else Nothing
parseInt (n : rest) = do
  case reads n :: [(Integer, String)] of
    [(f, "")] -> return (Num f, rest)
    _ -> return (VarExp n, rest)
parseInt _ = Nothing


----- Bexp ----

parseBaseBooleanExpression :: [String] -> Maybe (Bexp, [String])
parseBaseBooleanExpression tokens = case tokens of
  ("(" : remainingTokens) -> do
    let maybeExpr = parseAndExpression remainingTokens
    case maybeExpr of
      Just (expr, restString1) ->
        if head restString1 == ")"
          then Just (expr, tail restString1)
          else Nothing
      Nothing -> Nothing
  ("True" : remainingTokens) -> Just (TruB, remainingTokens)
  ("False" : remainingTokens) -> Just (FalsB, remainingTokens)
  remainingTokens -> do
    let maybeExpr1 = parseExpression remainingTokens [("+", AddExp), ("-", SubExp), ("*", MultExp)]
    case maybeExpr1 of
      Just (expression1, "<=" : remainingTokensAfterOperator) -> do
        let maybeExpr2 = parseExpression remainingTokensAfterOperator [("+", AddExp), ("-", SubExp), ("*", MultExp)]
        case maybeExpr2 of
          Just (expression2, remainingTokensAfterExpression) -> Just (LeExp expression1 expression2, remainingTokensAfterExpression)
          Nothing -> Nothing
      Just (expression1, "==" : remainingTokensAfterOperator) -> do
        let maybeExpr2 = parseExpression remainingTokensAfterOperator [("+", AddExp), ("-", SubExp), ("*", MultExp)]
        case maybeExpr2 of
          Just (expression2, remainingTokensAfterExpression) -> Just (EquAExp expression1 expression2, remainingTokensAfterExpression)
          Nothing -> Nothing
      _ -> Nothing

parseNegationExpression :: [String] -> Maybe (Bexp, [String])
parseNegationExpression tokens = case tokens of
  ("not" : remainingTokens) -> do
    let maybeExpr = parseBaseBooleanExpression remainingTokens
    case maybeExpr of
      Just (expression, remainingTokensAfterExpression) -> Just (NegExp expression, remainingTokensAfterExpression)
      Nothing -> Nothing
  remainingTokens -> parseBaseBooleanExpression remainingTokens

parseEqualityExpression :: [String] -> Maybe (Bexp, [String])
parseEqualityExpression tokens = do
  let maybeExpr1 = parseNegationExpression tokens
  case maybeExpr1 of
    Just (expression1, "=" : remainingTokensAfterOperator) -> do
      let maybeExpr2 = parseEqualityExpression remainingTokensAfterOperator
      case maybeExpr2 of
        Just (expression2, remainingTokensAfterExpression) -> Just (EquBoExp expression1 expression2, remainingTokensAfterExpression)
        Nothing -> Nothing
    _ -> maybeExpr1

parseAndExpression :: [String] -> Maybe (Bexp, [String])
parseAndExpression tokens = do
  let maybeExpr1 = parseEqualityExpression tokens
  case maybeExpr1 of
    Just (expression1, "and" : remainingTokensAfterOperator) -> do
      let maybeExpr2 = parseAndExpression remainingTokensAfterOperator
      case maybeExpr2 of
        Just (expression2, remainingTokensAfterExpression) -> Just (AndExp expression1 expression2, remainingTokensAfterExpression)
        Nothing -> Nothing
    _ -> maybeExpr1




-- tests :: [(Code, (String, String))]
-- tests =
--   [ ([Push 10, Push 4, Push 3, Sub, Mult], ("-10", "")),
--     ([Fals, Push 3, Tru, Store "var", Store "a", Store "someVar"], ("", "a=3,someVar=False,var=True")),
--     ([Fals, Store "var", Fetch "var"], ("False", "var=False")),
--     ([Push (-20), Tru, Fals], ("False,True,-20", "")),
--     ([Push (-20), Tru, Tru, Neg], ("False,True,-20", "")),
--     ([Push (-20), Tru, Tru, Neg, Equ], ("False,-20", "")),
--     ([Push (-20), Push (-21), Le], ("True", "")),
--     ([Push 5, Store "x", Push 1, Fetch "x", Sub, Store "x"], ("", "x=4")),
--     ([Push 10, Store "i", Push 1, Store "fact", Loop [Push 1, Fetch "i", Equ, Neg] [Fetch "i", Fetch "fact", Mult, Store "fact", Push 1, Fetch "i", Sub, Store "i"]], ("", "fact=3628800,i=1")),
--     ([Push 10, Store "n", Push 0, Store "a", Push 1, Store "b", Loop [Push 1, Fetch "n", Equ, Neg] [Fetch "a", Fetch "b", Add, Store "temp", Fetch "b", Store "a", Fetch "temp", Store "b", Push 1, Fetch "n", Sub, Store "n"]], ("", "a=34,b=55,n=1,temp=55"))
    
--   ]

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where
    (_, stack, state) = run (compile (parse programCode), createEmptyStack, createEmptyState)

-- parserTests :: [(String, (String, String))]
-- parserTests = 
--   [ ("x := 5; x := x - 1;", ("", "x=4")),
--     ("x := 0 - 2;", ("", "x=-2")),
--     ("if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;", ("", "y=2")),
--     ("x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);", ("", "x=1")),
--     ("x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;", ("", "x=2")),
--     ("x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;", ("", "x=2,z=4")),
--     ("x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;", ("", "x=34,y=68")),
--     ("x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;", ("", "x=34")),
--     ("if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;", ("", "x=1")),
--     ("if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;", ("", "x=2")),
--     ("x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);", ("", "x=2,y=-10,z=6")),
--     ("i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);", ("", "fact=3628800,i=1"))

--   ]


--runParserTests :: [(String, (String, String))] -> IO ()
--runParserTests [] = return ()
--runParserTests ((code, expected) : rest) = do
  --let result = testParser code
  --if result == expected
    --then putStrLn $ "Test passed: " ++ code
    --else putStrLn $ "Test failed: " ++ code ++ "\nExpected: " ++ show expected ++ "\nGot: " ++ show result
  --runParserTests rest


--main :: IO ()
--main = do
  --putStrLn "Running assembler tests..."
  --runTests tests
  --putStrLn "Running parser tests..."
  --runParserTests parserTests

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where
    (_, stack, state) = run (code, createEmptyStack, createEmptyState)

--runTests :: [(Code, (String, String))] -> IO ()
--runTests [] = return ()
--runTests ((code, expected) : rest) = do
  --let result = testAssembler code
  --if result == expected
    --then putStrLn $ "Test passed: " ++ show code
    --else putStrLn $ "Test failed: " ++ show code ++ "\nExpected: " ++ show expected ++ "\nGot: " ++ show result
  --runTests rest
