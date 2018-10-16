{-# OPTIONS_GHC -Wall #-}

module Lesson41 where 

import Prelude

myRepicate :: Int -> a -> [a]
myRepicate x y = [y | _ <- [1..x]]


fun :: [[(Integer, Integer)]]
fun = [[(x,y) | y<- [3,4]] | x<-[1,2]]

myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop _ [] = []
myDrop n (_:xs) =myDrop (n-1) xs

main :: IO()
main = do
    print ("abcde" !! 2)
    print (myRepicate 3 "ciao")
    print fun  -- Ex 7 pag 9
    print (myDrop 2 "asdf")