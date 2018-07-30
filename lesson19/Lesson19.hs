module Lesson19 where
import qualified Data.Map as M
import qualified Data.Maybe as Maybe

data Organ = Heart | Brain | Spleen | Kidney

getDrawerContents :: [Int] -> M.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
  where getContents = \id -> M.lookup id catalog

emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers = length . filter Maybe.isNothing

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f (Just x) = Just $ f x
maybeMap f Nothing = Nothing
