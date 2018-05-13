myGCD:: Int -> Int -> Int
myGCD a 0 = a
myGCD a b = myGCD b (a `mod` b)

myTail (_:xs) = xs
