
reverseSlow [] = []
reverseSlow (x : xs) = reverseSlow xs ++ [x]

main = do
    print (reverseSlow [1,2,3,4,5])
