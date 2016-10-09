module Expr where

-- code between "----" markers are not part of the assignment

import Test.QuickCheck
import Data.List

-- A

-- Data types, is "Var Name" correct?

data Expr =  Num Float
           | Add Expr Expr
           | Mul Expr Expr
           | Sin Expr
           | Cos Expr
           | Var Name
    deriving Eq

type Name = String

-- Do we need this when it's only going to be one variable, "x"?

vars :: Expr -> [Name]
vars (Num n)   = []
vars (Add a b) = vars a `union` vars b
vars (Mul a b) = vars a `union` vars b
vars (Sin a)   = vars a -- Is this correct?
vars (Cos a)   = vars a -- Is this correct?
vars (Var x)   = [x]

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
showExpr (Var x) = show x -- Is this correct? probably not

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

-- E

-- prop_ShowReadExpr :: Expr -> Bool

-- arbExpr :: Int -> Gen Expr

-- instance Arbitrary Expr where
--     arbitrary = sized arbExpr
