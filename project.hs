import Data.Char (isAlpha, isAlphaNum, isDigit, isNumber, isSpace)
import Data.List (deleteBy, elemIndex, intercalate, isPrefixOf, sort)
import Data.Maybe (fromMaybe)
import Debug.Trace
import Text.Read (readMaybe)

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

data Aexp = Num Integer | Var String | AddA Aexp Aexp | SubA Aexp Aexp | MultA Aexp Aexp deriving (Show)

data Bexp = EquB Aexp Aexp | LeB Aexp Aexp | AndB Bexp Bexp | EquBoolB Bexp Bexp | NegB Bexp | TruB | FalsB deriving (Show)

data Stm = BranchS Bexp [Stm] [Stm] | LoopS Bexp [Stm] | VarAssign String Aexp deriving (Show)

type Program = [Stm]

compA :: Aexp -> Code
compA (Num a) = [Push a]
compA (Var a) = [Fetch a]
compA (AddA a b) = compA b ++ compA a ++ [Add]
compA (SubA a b) = compA b ++ compA a ++ [Sub]
compA (MultA a b) = compA b ++ compA a ++ [Mult]

compB :: Bexp -> Code
compB (EquB a b) = compA b ++ compA a ++ [Equ]
compB (LeB a b) = compA b ++ compA a ++ [Le]
compB (AndB a b) = compB b ++ compB a ++ [And]
compB (NegB a) = compB a ++ [Neg]
compB (EquBoolB a b) = compB b ++ compB a ++ [Equ]
compB TruB = [Tru]
compB FalsB = [Fals]

compile :: Program -> Code
compile = concatMap compileStm

compileStm :: Stm -> Code
compileStm stm = case stm of
  VarAssign var aexp -> compA aexp ++ [Store var]
  BranchS bexp stm1 stm2 -> compB bexp ++ [Branch (compile stm1) (compile stm2)]
  LoopS bexp stm -> [Loop (compB bexp) (compile stm)]

parse :: String -> Program
parse str = parseAuxiliary (lexer str) []

parseAuxiliary :: [String] -> [Stm] -> [Stm]
parseAuxiliary [] statements = statements
parseAuxiliary (token : ":=" : remainingTokens) statements =
  let (beforeAssignment, afterAssignment) = break (== ";") (token : ":=" : remainingTokens)
   in case parseSumOrProdOrIntOrPar (drop 2 beforeAssignment) of
        Just (expression, []) -> parseAuxiliary (drop 1 afterAssignment) (statements ++ [VarAssign token expression])
        Nothing -> error "Parse Error"
        _ -> error "Parse Error"
parseAuxiliary ("(" : remainingTokens) statements =
  let (beforeClosingParen, afterClosingParen) = break (== ")") ("(" : remainingTokens)
   in parseAuxiliary (drop 1 afterClosingParen) (statements ++ parseAuxiliary (drop 1 beforeClosingParen) [])
parseAuxiliary (";" : remainingTokens) statements = parseAuxiliary remainingTokens statements
parseAuxiliary ("if" : remainingTokens) statements =
  let (beforeThen, afterThen) = break (== "then") ("if" : remainingTokens)
      (beforeElse, afterElse) = break (== "else") afterThen
      tokensAfterElse = drop 1 afterElse
   in case takeFirstElement tokensAfterElse of
        "(" ->
          let (beforeClosingParen, afterClosingParen) = break (== ")") tokensAfterElse
           in parseAuxiliary (drop 1 afterClosingParen) (statements ++ [BranchS (getJustValueBexp (parseAndandBoolEq (checkIfParenthesis (drop 1 beforeThen)))) (parseAuxiliary (drop 1 beforeElse) []) (parseAuxiliary beforeClosingParen [])])
        _ ->
          let (beforeSemicolon, afterSemicolon) = break (== ";") tokensAfterElse
           in parseAuxiliary (drop 1 afterSemicolon) (statements ++ [BranchS (getJustValueBexp (parseAndandBoolEq (checkIfParenthesis (drop 1 beforeThen)))) (parseAuxiliary (drop 1 beforeElse) []) (parseAuxiliary beforeSemicolon [])])
parseAuxiliary ("while" : remainingTokens) statements =
  let (beforeDo, afterDo) = break (== "do") ("while" : remainingTokens)
      tokensAfterDo = drop 1 afterDo
   in case takeFirstElement tokensAfterDo of
        "(" ->
          let (beforeClosingParen, afterClosingParen) = break (== ")") tokensAfterDo
           in parseAuxiliary (drop 1 afterClosingParen) (statements ++ [LoopS (getJustValueBexp (parseAndandBoolEq (checkIfParenthesis (drop 1 beforeDo)))) (parseAuxiliary beforeClosingParen [])])
        _ ->
          let (beforeSemicolon, afterSemicolon) = break (== ";") tokensAfterDo
           in parseAuxiliary (drop 1 afterSemicolon) (statements ++ [LoopS (getJustValueBexp (parseAndandBoolEq (checkIfParenthesis (drop 1 beforeDo)))) (parseAuxiliary beforeSemicolon [])])

getJustValueBexp :: Maybe (Bexp, [String]) -> Bexp
getJustValueBexp (Just (booleanExpression, _)) = booleanExpression
getJustValueBexp Nothing = error "Parse Error"

checkIfParenthesis :: [String] -> [String]
checkIfParenthesis ("(" : remainingTokens) = remainingTokens
checkIfParenthesis remainingTokens = remainingTokens

takeFirstElement :: [String] -> String
takeFirstElement (firstElement : _) = firstElement
parseInt :: [String] -> Maybe (Aexp, [String])
parseInt (n : rest) =
  case (readMaybe n :: Maybe Integer) of
    Just f -> Just (Num f, rest)
    Nothing -> Just (Var n, rest)
parseInt _ = Nothing

parseProductOrInteger :: [String] -> Maybe (Aexp, [String])
parseProductOrInteger tokens =
  parseInt tokens >>= \(firstExpression, remainingTokens) ->
    if null remainingTokens || head remainingTokens /= "*"
      then Just (firstExpression, remainingTokens)
      else parseProductOrInteger (tail remainingTokens) >>= \(secondExpression, remainingTokensAfterSecondExpression) ->
        Just (MultA firstExpression secondExpression, remainingTokensAfterSecondExpression)

parseSumOrProdOrInt :: [String] -> Maybe (Aexp, [String])
parseSumOrProdOrInt str =
  case parseProductOrInteger str of
    Just (expr1, "+" : restString1) ->
      case parseSumOrProdOrInt restString1 of
        Just (expr2, restString2) ->
          Just (AddA expr1 expr2, restString2)
        Nothing -> Nothing
    Just (expr1, "-" : restString1) ->
      case parseSumOrProdOrInt restString1 of
        Just (expr2, restString2) ->
          Just (SubA expr1 expr2, restString2)
        Nothing -> Nothing
    result -> result

parseIntOrParentExpr :: [String] -> Maybe (Aexp, [String])
parseIntOrParentExpr ("(" : rest) =
  case parseSumOrProdOrIntOrPar rest of
    Just (expr, ")" : restString1) -> Just (expr, restString1)
    Just _ -> Nothing
    Nothing -> Nothing
parseIntOrParentExpr (n : rest) =
  case (readMaybe n :: Maybe Integer) of
    Just f -> Just (Num f, rest)
    Nothing -> Just (Var n, rest)
parseIntOrParentExpr _ = Nothing

parseProdOrIntOrPar :: [String] -> Maybe (Aexp, [String])
parseProdOrIntOrPar rest =
  case parseIntOrParentExpr rest of
    Just (expr1, "*" : restString1) ->
      case parseProdOrIntOrPar restString1 of
        Just (expr2, restString2) -> Just (MultA expr1 expr2, restString2)
        Nothing -> Nothing
    result -> result

parseSumOrProdOrIntOrPar :: [String] -> Maybe (Aexp, [String])
parseSumOrProdOrIntOrPar rest =
  case parseProdOrIntOrPar rest of
    Just (expr1, "+" : restString1) ->
      case parseSumOrProdOrIntOrPar restString1 of
        Just (expr2, restString2) -> Just (AddA expr1 expr2, restString2)
        Nothing -> Nothing
    Just (expr1, "-" : restString1) ->
      case parseSumOrProdOrIntOrPar restString1 of
        Just (expr2, restString2) -> Just (SubA expr1 expr2, restString2)
        Nothing -> Nothing
    result -> result

------------- PARSE Bexp ----------------

parseLessOrEqOrTrueOrFalseOrParentOrArith :: [String] -> Maybe (Bexp, [String])
parseLessOrEqOrTrueOrFalseOrParentOrArith ("(" : rest) =
  case parseAndandBoolEq rest of
    Just (expr, ")" : restString1) -> Just (expr, restString1)
    Just _ -> Nothing
    Nothing -> Nothing
parseLessOrEqOrTrueOrFalseOrParentOrArith ("True" : rest) = Just (TruB, rest)
parseLessOrEqOrTrueOrFalseOrParentOrArith ("False" : rest) = Just (FalsB, rest)
parseLessOrEqOrTrueOrFalseOrParentOrArith rest =
  case parseSumOrProdOrIntOrPar rest of
    Just (expr1, "<=" : restString1) ->
      case parseSumOrProdOrIntOrPar restString1 of
        Just (expr2, restString2) ->
          Just (LeB expr1 expr2, restString2)
        Nothing -> Nothing
    Just (expr1, "==" : restString1) ->
      case parseSumOrProdOrIntOrPar restString1 of
        Just (expr2, restString2) ->
          Just (EquB expr1 expr2, restString2)
        Nothing -> Nothing
    result -> Nothing

parseNegAndLessAndEq :: [String] -> Maybe (Bexp, [String])
parseNegAndLessAndEq ("not" : rest) =
  case parseLessOrEqOrTrueOrFalseOrParentOrArith rest of
    Just (expr1, restString1) ->
      Just (NegB expr1, restString1)
    result -> result
parseNegAndLessAndEq rest = parseLessOrEqOrTrueOrFalseOrParentOrArith rest

parseBoolEqAndNeg :: [String] -> Maybe (Bexp, [String])
parseBoolEqAndNeg rest =
  case parseNegAndLessAndEq rest of
    Just (expr1, "=" : restString1) ->
      case parseBoolEqAndNeg restString1 of
        Just (expr2, restString2) ->
          Just (EquBoolB expr1 expr2, restString2)
        Nothing -> Nothing
    result -> result

parseAndandBoolEq :: [String] -> Maybe (Bexp, [String])
parseAndandBoolEq rest =
  case parseBoolEqAndNeg rest of
    Just (expr1, "and" : restString1) ->
      case parseAndandBoolEq restString1 of
        Just (expr2, restString2) ->
          Just (AndB expr1 expr2, restString2)
        Nothing -> Nothing
    result -> result



-----------------------------------------


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
    ( [ Push 10,
        Store "n",
        Push 1,
        Store "result",
        Loop
          [Push 1, Fetch "n", Equ, Neg]
          [Fetch "n", Fetch "result", Mult, Store "result", Push 1, Fetch "n", Sub, Store "n"]
      ],
      ("", "n=1,result=3628800")
    )
  ]

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where
    (_, stack, state) = run (compile (parse programCode), createEmptyStack, createEmptyState)

parserTests :: [(String, (String, String))]
parserTests =
  [ ("x := 5; x := x - 1;", ("", "x=4")),
    ("x := 0 - 2;", ("", "x=-2")),
    ("if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;", ("", "y=2")),
    ("x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);", ("", "x=1")),
    ("x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;", ("", "x=2")),
    ("x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;", ("", "x=2,z=4")),
    ("x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;", ("", "x=34,y=68")),
    ("x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;", ("", "x=34")),
    ("if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;", ("", "x=1")),
    ("if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;", ("", "x=2")),
    ("x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);", ("", "x=2,y=-10,z=6")),
    ("i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);", ("", "fact=3628800,i=1"))
  ]

-- Function to run all parser tests
runParserTests :: [(String, (String, String))] -> IO ()
runParserTests [] = return ()
runParserTests ((code, expected) : rest) = do
  let result = testParser code
  if result == expected
    then putStrLn $ "Test passed: " ++ code
    else putStrLn $ "Test failed: " ++ code ++ "\nExpected: " ++ show expected ++ "\nGot: " ++ show result
  runParserTests rest

-- Run all tests
main :: IO ()
main = do
  putStrLn "Running assembler tests..."
  runTests tests
  putStrLn "Running parser tests..."
  runParserTests parserTests

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where
    (_, stack, state) = run (code, createEmptyStack, createEmptyState)

-- Function to run all tests
runTests :: [(Code, (String, String))] -> IO ()
runTests [] = return ()
runTests ((code, expected) : rest) = do
  let result = testAssembler code
  if result == expected
    then putStrLn $ "Test passed: " ++ show code
    else putStrLn $ "Test failed: " ++ show code ++ "\nExpected: " ++ show expected ++ "\nGot: " ++ show result
  runTests rest
