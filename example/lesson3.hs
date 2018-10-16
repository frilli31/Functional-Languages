{-# OPTIONS_GHC -Wall #-}

module Lesson3 where 

fn :: [Char] -> IO ()
fn name = do  
    putStrLn "Hello, what's your name?"
    putStrLn ("Hey " ++ name ++ ", you rock!")

twice :: (t -> t) -> t -> t
twice f x = f(f x)

even :: Integer -> Bool
even n = n `mod` 2 == 0

ifThenElse :: Bool -> IO ()
ifThenElse condition = 
    if condition          -- If statement must have both then and else end must have the same type
        then putStrLn "True"
        else putStrLn "False"


myAbs :: Integer -> Integer
myAbs n | n>=0 = n
      | otherwise = -n


mySignum :: Integer -> Integer
mySignum n | n>0 = 1
         | n==0 = 0
         | otherwise = -1

myAnd :: Bool -> Bool -> Bool
myAnd x y | x==True && y==True = True
          | otherwise = False


-- Pattern Matching Example
mySecondEnd :: Bool -> Bool -> Bool
mySecondEnd True True = True
mySecondEnd _ _ = False


test :: [Char] -> Bool
test ['a', _, _] = True
test _ = False

thirdWithExclamation :: [a] -> a
thirdWithExclamation list = list !! 2

thirdWithHeadTail :: [a] -> a
thirdWithHeadTail list = head (tail (tail list))

thirdWithPaternMatching :: [a] -> a
thirdWithPaternMatching (n:ns:nss:[])= nss

{-define safetail :: [a]->[a] 
that behaves like tail except that with [], insted of failing, answers []. 
We can use tail and null :: [a] -> Bool that answers True iff the input is an empty list, 
define safetail using: a) a conditional expression b) guarded equations c) pattern matching
-}
safetailWithCondExpr :: [a]->[a]
safetailWithCondExpr list =
    if null list
        then []
        else tail list

safetailWithCondExpr list | list == [] = []
                          | list = tail list

safetailWithPatternMatch [] = []
safetailWithPatternMatch (x:xs) = xs

main :: IO()
main = do
    ifThenElse(True)
    fn "Luca"
    print (myAbs (-4))
    print (mySignum (-100))
    print (myAnd True True)
    print (myAnd True False)
    print (mySecondEnd True False)
    print (test ['b'])              -- False
    print (test ['a', 'b', 'c'])    -- True
    print (test "abc")              -- True
    print (test "abcd")             -- False
    print (thirdWithExclamation [1,2,3])
    print (thirdWithHeadTail [1,2,3])
    print (thirdWithPaternMatching [1,2,3])
    print (safetailWithCondExpr [1,2,3,4])