mylast :: [a] -> Maybe a
mylast [] = Nothing
mylast (x:[]) = Just x
mylast (x:xs) = mylast xs
