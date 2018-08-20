module Lesson21 where

import qualified Data.Map as M

names :: M.Map Int String
names = M.fromList [(1, "Peter"), (2, "James"), (3, "Mark")]

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

helloMap :: M.Map Int String -> Int -> Maybe String
helloMap m k = do
    name <- M.lookup k m
    return $ helloPerson name


fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
    print "Fibonacci number index: "
    n <- read <$> getLine
    let fibNum = head $ drop n fibs
    print fibNum
