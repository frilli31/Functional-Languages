{-# OPTIONS_GHC -Wall #-}

qsort[] = []
qsort(x : xs) = qsort smaller ++ [x] ++ qsort(larger)
    where
        smaller = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]

main :: IO ()
main = print (qsort [7,3,9,6,99])