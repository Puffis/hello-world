module Expr where

-- code between "----" markers are not part of the assignment

--tjaaaaaaa

import Test.QuickCheck

--------------------------
import Test.QuickCheck.Gen
import System.Random
--------------------------

-- Data types, need to add types for cos- and sin- functions?? 

data Expr =  Num Float
           | Add Expr Expr
           | Mul Expr Expr
           | Floating Expr
    deriving Eq

-- instance so we can show expressions

instance Show Expr where
    show = showExpr

eval :: Expr -> Float
eval (Num n)   = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

showExpr :: Expr -> String
showExpr (Num n)   = show n
showExpr (Add a b) = showExpr a ++ " + " ++ showExpr b
showExpr (Mul a b) = showFactor a ++ " * " ++ showFactor b

showFactor :: Expr -> String
showFactor (Add a b) = "(" ++ showExpr (Add a b) ++ ")"
showFactor e         = showExpr e

------------------------------------------------------

{-
questions :: IO ()
questions = do
            e <- generate arbitrary
            putStr ("What is " ++ show e ++ "?")
            ans <- getLine
            putStrLn (if read ans == eval e
                        then "Correct!" 
                        else "Wrong!")

generate :: Gen a -> IO a
generate g = do
    seed <- newStdGen
    return (unGen g seed 10)

instance Arbitrary Expr where
    arbitrary = arbExpr

arbExpr :: Gen Expr
arbExpr = 
    frequency [(1, do n <- arbitrary
                      return (Num n))
              ,(s, do a <- arbExpr s'
                      b <- arbExpr s'
                      return (Add a b))
              ,(s, do a <- arbExpr s'
                      b <- arbExpr s'
                      return (Mul a b)) ]
        where
          s' = s ´div´ 2

------------------------------------------------------
-}
