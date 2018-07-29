module Lesson18 where
import qualified Data.Map as M
import qualified Data.List as L

data Box a = Box a deriving (Show)
data Triple a = Triple a a a deriving (Show)

tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

boxMap f (Box x) = Box $ f x

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord)

ids :: [Int]
ids = [2,7,13,14,21,24]

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

organCatalog :: M.Map Int Organ
organCatalog = M.fromList(organPairs)

organInventory :: M.Map Organ Int
organInventory = M.fromList $ zip uniqueOrgans organCounts
  where sorgans = L.sort organs
        uniqueOrgans = L.nub sorgans
        organCounts = map length $ L.group sorgans
