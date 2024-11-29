module Main where

import Data.Char (isDigit);

data Op = Inc
        | Dec
        | Mul
        | Div
        deriving (Show, Eq);

data Token = Number Float
           | Open
           | Close
           | Exp Op
           | EOF
           deriving (Show);

data AST = Node Float
         | OpNode Op AST AST
         deriving (Show);

data OpMaybe = OpJust Op
             | OpNothing
             deriving (Show);

getOp :: Char -> Op
getOp x
  | x == '+' = Inc
  | x == '-' = Dec
  | x == '/' = Div
  | x == '*' = Mul
  | otherwise = error "underfined token"

untokenOp :: Fractional a => Op -> a -> a -> a
untokenOp (op)
  | op == Inc = (+)
  | op == Dec = (-)
  | op == Mul = (*)
  | op == Div = (/)
  | otherwise = error "Unexpected token"

isWp :: Char -> Bool
isWp x = x `elem` [' ', '\n', '\t'];

isNum :: Char -> Bool
isNum x = isDigit x || x == '.'

isOp :: Char -> OpMaybe
isOp x
  | x `elem` ['+', '-', '/', '*'] = OpJust $ getOp x
  | otherwise = OpNothing

lexer :: String -> [Token]
lexer [] = [EOF]
lexer ('(':xs) = Open:lexer xs
lexer (')':xs) = Close:lexer xs
lexer (x:xs)
  | isWp x = lexer xs
  | isNum x = let (num, rest) = span (isNum) (x:xs)
              in Number (read num):lexer rest
  | otherwise = case isOp x of
    OpJust op -> Exp op:lexer xs
    OpNothing -> error "underfined token"

parseToAst :: [Token] -> AST
parseToAst ts =
  let (ast, rest) = parseExpr ts
  in case (rest) of
    [EOF] -> ast
    _ -> error "Unexpecter error while buliding AST"

parseExpr :: [Token] -> (AST, [Token])
parseExpr ts =
  let (term, rest) = parseFac ts
  in parseExpr' term rest

parseExpr' :: AST -> [Token] -> (AST, [Token])
parseExpr' lhs (Exp op:ts) =
  let (rhs, rest) = parseTerm ts
      nLhs = OpNode op lhs rhs
  in parseExpr' nLhs rest
parseExpr' ast ts = (ast, ts)

parseTerm :: [Token] -> (AST, [Token])
parseTerm t =
  let (fac, rest) = parseFac t
  in parseTerm' fac rest

parseTerm' :: AST -> [Token] -> (AST, [Token])
parseTerm' lhs (Exp Mul:ts) =
  let (rhs, rest) = parseFac ts
      nLhs = OpNode Mul lhs rhs
  in parseTerm' nLhs rest
parseTerm' lhs (Exp Div:ts) =
  let (rhs, rest) = parseFac ts
      nLhs = OpNode Div lhs rhs
  in parseTerm' nLhs rest
parseTerm' lhs ts = (lhs, ts)

parseFac :: [Token] -> (AST, [Token])
parseFac (Number n:ts) = (Node n, ts)
parseFac (Open:ts) = let (ast, rest) = parseExpr ts
                     in case rest of
                      (Close:ts')-> (ast, ts')
                      _          -> error "Unclosed ("
parseFac ts = error $ "Unexpected token in" ++ show ts

calculate :: AST -> Float
calculate (Node num) = num
calculate (OpNode op lhs rhs) = (untokenOp op) (calculate lhs) (calculate rhs)

main :: IO ()
main = do
  putStrLn "--- C A L C U L A T O R ---"
  inp <- lexer <$> getLine
  putStrLn $ show $  calculate $ parseToAst inp