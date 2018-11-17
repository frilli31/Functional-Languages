{-# OPTIONS_GHC -Wall #-}

module Calc where

import Prelude  
import ExprT
import Parser

testExp :: Expr a => String -> Maybe a
testExp = parseExp lit add mul 

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y


evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
    Just x -> Just (eval x)
    Nothing -> Nothing

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a
      
instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Bool where
    lit a = if a>0 then True else False
    add = (||)
    mul = (&&)

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr MinMax where
    lit x = MinMax x
    add (MinMax a) (MinMax b) = if a >= b then MinMax a else MinMax b
    mul (MinMax a) (MinMax b) = if a < b then MinMax a else MinMax b
    
instance Expr Mod7 where
    lit x = Mod7 x
    add (Mod7 a) (Mod7 b) = Mod7 ((a+b) `mod` 7)
    mul (Mod7 a) (Mod7 b) = Mod7((a*b) `mod` 7)
    
    
      
reify :: ExprT -> ExprT
reify = id


main :: IO ()
main = do
    print (eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)))
    print (evalStr "2+3*4")