{-# OPTIONS_GHC -Wall #-}

module Lesson4 where 

myConcat :: [[Integer]] -> [Integer]
myConcat xss = [x | xs <- xss, x <- xs]


main :: IO()
main = do
    -- List Comprehension
    print [x^2 | x <-[1..5]]                -- x <-[1..5] is a generator
                                            -- print: [1,4,9,16,25]
    print (myConcat [[3,4],[37,8],[],[9]])  -- print: [3,4,37,8,9]
    
