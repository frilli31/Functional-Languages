{-# OPTIONS_GHC -Wall #-}

module Home3 where

import Prelude

ex1 :: [a] -> [[a]]
ex1 l = [[z | (z, i) <-zip l [1..], i `mod` n == 0] | n <- [1..length l]]

ex2 :: [Int] -> [Int]
ex2 l = [z | (z, i) <-zip l [0..], i>0, i<(length l-1), z>l!!(i-1), z>l!!(i+1)]

count :: Eq a => a -> [a] -> Int
count e = length . filter (e ==)

printLevel :: Int -> [Int] -> String
printLevel 0 _ = []
printLevel n l = [if el>=n then '*' else ' ' | el<-l] ++ "\n" ++ printLevel (n-1) l

ex3 :: [Int] -> String
ex3 l = printLevel (maximum occ) occ ++ "==========\n0123456789\n" where occ = [count i l | i <- [0..9]]