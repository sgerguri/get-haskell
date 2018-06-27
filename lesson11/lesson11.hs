halve :: Integer -> Integer
halve n = n `div` 2

printDouble :: Int -> String
printDouble x = show (x * 2)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where newInit = f init x
