module Lesson8 where

drop:: Int -> [a] -> [a]
drop _ [] = []
drop 0 xs = xs
drop n (_:xs) = Lesson8.drop (n - 1) xs

myLength [] = 0
myLength (_:xs) = 1 + myLength xs

reverse:: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]
