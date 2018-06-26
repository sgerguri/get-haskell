module Lesson9 where
import Data.Char (toLower)

elem :: (Eq a) => a -> [a] -> Bool
elem x xs = 0 /= (length $ filter (==x) xs)

isPalindrome :: String -> Bool
isPalindrome s = canonical == reverse canonical
  where canonical = map toLower $ filter (/=' ') s

harmonic 1 = 1
harmonic n = 1/n + harmonic (n-1)
