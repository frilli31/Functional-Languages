{-# OPTIONS_GHC -Wall #-}

module Lesson4 where 

import Prelude

myConcat :: [[Integer]] -> [Integer]
myConcat xss = [x | xs <- xss, x <- xs]

add :: (Num a) => a -> a -> a -> a
add = \x -> \y -> \z -> x+y+z

odds :: (Num b, Enum b) => b -> [b]
odds n = map (\x -> x*2+1) [0..n-1]

myReplicate :: Int -> a -> [a]
myReplicate n obj | n<=0 = []
                  | otherwise = obj : myReplicate (n-1) obj

factors n = [x | x <- [1..n], n `mod` x ==0]

prime n = factors n == [1,n]

primes n = [x | x <- [2..n], prime x] -- with prime as condition

-- find :: Eq a => a -> [(a,b)] -> [b]
-- find k t = [v | (k',v)<-t, k=k']                      -- in section 5 of book: Ceasar Cripthrography

main :: IO()
main = do
    -- List Comprehension
    print [x^2 | x <-[1..5]]                -- x <-[1..5] is a generator
                                            -- print: [1,4,9,16,25]
    print (myConcat [[3,4],[37,8],[],[9]])  -- print: [3,4,37,8,9]

    print (add 3 4 5)

    mapM print ["ciao","chiara","come","stai"]
    print (map length ["ciao","chiara","come","stai"])

    print (odds 10)

    print (myReplicate 5 "chiara")

