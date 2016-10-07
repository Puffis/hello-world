module Expr where
-- David du är snygg
-- code between "----" markers are not part of the assignment

--------------------------
import Test.QuickCheck.Gen
import System.Random
--------------------------

import Test.QuickCheck

-- A

-- Data types, need to add types for cos- and sin- functions?? 

data Expr =  Num Float
           | Add Expr Expr
           | Mul Expr Expr
    deriving Eq

-- B

-- instance so we can show expressions

instance Show Expr where
    show = showExpr

showExpr :: Expr -> String
showExpr (Num n)   = show n
showExpr (Add a b) = showExpr a ++ " + " ++ showExpr b
showExpr (Mul a b) = showExpr a ++ " * " ++ showExpr b

{-
showFactor :: Expr -> String
showFactor (Add a b) = "(" ++ showExpr (Add a b) ++ ")"
showFactor e         = showExpr e
-}

-- C

eval :: Expr -> Float
eval (Num n)   = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

-- D

-- comments?

readExpr :: String -> Maybe Expr
readExpr = undefined

-- E

-- comments?

-- prop_ShowReadExpr :: Expr -> Bool

-- arbExpr :: Int -> Gen Expr

-- instance Arbitrary Expr where
--     arbitrary = sized arbExpr


--Random code from here and on, ignore!
------------------------------------------------------

questions :: IO ()
questions = do
            e <- generate arbitrary
            putStr ("What is " ++ show e ++ "?")
            ans <- getLine
            putStrLn (if ans == show (eval e)
                        then "Correct!" 
                        else "Wrong!")

generate :: Gen a -> IO a
generate g = do
    seed <- newStdGen
    return (unGen g seed 10)

instance Arbitrary Expr where
    arbitrary = sized arbExpr

arbExpr :: Int -> Gen Expr
arbExpr s = 
    frequency [(1, do n <- arbitrary
                      return (Num n))
              ,(s, do a <- arbExpr s'
                      b <- arbExpr s'
                      return (Add a b))
              ,(s, do a <- arbExpr s'
                      b <- arbExpr s'
                      return (Mul a b)) ]
    where
      s' = s `div` 2

------------------------------------------------------
