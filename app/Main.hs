module Main where

import Data.Char (isDigit);

data Op = Inc
        | Dec
        | Mul
        | Div
        deriving (Show);

data Token = Number Float
           | Open
           | Close
           | Exp Op
           | EOF
           deriving (Show);

data AST = Node Float
         | OpNode Op Float Float
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

main :: IO ()
main = do
  inp <- getLine
  putStrLn $ show $ lexer inp