{-# OPTIONS_GHC -Wall #-}

fn :: [Char] -> IO ()
fn name = do  
    putStrLn "Hello, what's your name?"
    putStrLn ("Hey " ++ name ++ ", you rock!")

twice :: (t -> t) -> t -> t
twice f x = f(f x)

even :: Integral a => a -> Bool
even n = n `mod` 2 == 0

ifThenElse :: Bool -> IO ()
ifThenElse condition = 
    if condition          -- If statement must have both then and else end must have the same type
        then putStrLn "True"
        else putStrLn "False"

abs :: (Ord p, Num p) => p -> p
abs n | n>=0 = n
      | otherwise = -n


main :: IO()
main = do
    ifThenElse(True)
    fn "Luca"