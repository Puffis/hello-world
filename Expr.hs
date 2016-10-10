module Expr where

import Test.QuickCheck
import Data.List
import Data.Char

-- A

-- Data types, is "Var Name" correct?

data Expr =  Num Float
           | Add Expr Expr
           | Mul Expr Expr
           | Sin Expr
           | Cos Expr
           | Var
    deriving Eq

type Name = String

-- B

-- instance so we can show expressions

instance Show Expr where
    show = showExpr

showExpr :: Expr -> String
showExpr (Num n)   = show n
showExpr (Add a b) = showExpr a ++ " + " ++ showExpr b
showExpr (Mul a b) = showFactor a ++ " * " ++ showFactor b
showExpr (Sin a)   = "sin " ++ showTrig a
showExpr (Cos a)   = "cos " ++ showTrig a
showExpr (Var)     = "x" -- ???

showFactor :: Expr -> String
showFactor (Add a b) = "(" ++ showExpr (Add a b) ++ ")"
showFactor e         = showExpr e

showTrig :: Expr -> String
showTrig (Add a b) = "(" ++ showExpr (Add a b) ++ ")"
showTrig (Mul a b) = "(" ++ showExpr (Mul a b) ++ ")"
showTrig e         = showExpr e

-- C

-- How to make this look like eval :: Expr -> Double -> Double? How to add variable?

eval :: Expr -> Float
eval (Num n)   = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b
eval (Sin a)   = sin (eval a)
eval (Cos a)   = cos (eval a)

-- D

type Parser a = String -> Maybe (a, String)

readExpr :: String -> Maybe Expr
readExpr s = case expr s of
               Just(a, "") -> Just a
               _           -> Nothing

chain p op f s =
   case p s of
     Just (n,c:s')| c == op ->
            case chain p op f s' of
                Just (m,s'') -> Just (f n m,s'')
                Nothing -> Just (n,c:s')
     r -> r

expr, term :: Parser Expr
expr = chain term '+' Add
term = chain factor '*' Mul

factor :: Parser Expr
factor ('(':s) =
  case expr s of
    Just (a, ')':s1) -> Just (a,s1)
    _                -> Nothing

factor s = num s


num :: Parser Expr
num s = case number s of
    Just (n,s1) -> Just (Num n, s1)
    Nothing     -> Nothing



number :: Parser Float
-- number "123+4" == Just(123,"+4")
number ('-':cs) = case number cs of
   Just(n,s) -> Just(-n, s)
   _         -> Nothing


number (c:cs) | isDigit c = Just (read digits,rest)
  where
  digits = c:takeWhile isDigit cs
  rest   = dropWhile isDigit cs
number _                  = Nothing

-- E

-- prop_ShowReadExpr :: Expr -> Bool

-- arbExpr :: Int -> Gen Expr

-- instance Arbitrary Expr where
--     arbitrary = sized arbExpr
