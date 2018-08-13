module TimeSeries where

import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

file1 :: [(Int, Double)]
file1 = [
  (1, 200.1),
  (2, 199.5),
  (3, 199.4),
  (4, 198.9),
  (5, 199.0),
  (6, 200.2),
  (9, 200.3),
  (10, 201.2),
  (12, 202.9)
  ]

file2 :: [(Int, Double)]
file2 = [
  (11, 201.6),
  (12, 201.5),
  (13, 201.5),
  (14, 203.5),
  (15, 204.9),
  (16, 207.1),
  (18, 210.5),
  (20, 208.8)
  ]

file3 :: [(Int, Double)]
file3 = [
  (10, 201.2),
  (11, 201.6),
  (12, 201.5),
  (13, 201.5),
  (14, 203.5),
  (17, 210.5),
  (24, 215.1),
  (25, 218.7)
  ]

file4 :: [(Int, Double)]
file4 = [
  (26, 219.8),
  (27, 220.5),
  (28, 223.8),
  (29, 222.8),
  (30, 223.8),
  (31, 221.7),
  (32, 222.3),
  (33, 220.8),
  (34, 219.4),
  (35, 220.1),
  (36, 220.6)
  ]

data TS a = TS [Int] [Maybe a]

createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues
  where completeTimes = [minimum times .. maximum times]
        timeValueMap = Map.fromList $ zip times values
        extendedValues = map (`Map.lookup` timeValueMap) completeTimes

fileToTS :: [(Int,a)] -> TS a
fileToTS = uncurry createTS . unzip

showTVPair :: Show a => Int -> Maybe a -> String
showTVPair time (Just value) = mconcat [show time,"|",show value,"\n"]
showTVPair time Nothing = mconcat [show time,"|NA\n"]

instance Show a => Show (TS a) where
  show (TS times values) = mconcat rows
    where rows = zipWith showTVPair times values

ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing) = myMap
insertMaybePair myMap (key, Just value) = Map.insert key value myMap

combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) t2 = t2
combineTS t1 (TS [] []) = t1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
  where bothTimes = mconcat [t1, t2]
        completeTimes = [minimum bothTimes .. maximum bothTimes]
        tvMap = foldl insertMaybePair Map.empty $ mappend (zip t1 v1) (zip t2 v2)
        combinedValues = map (`Map.lookup` tvMap) completeTimes

instance Semigroup (TS a) where
  (<>) = combineTS

instance Monoid (TS a) where
  mempty = TS [] []
  mappend = (<>)

tsAll :: TS Double
tsAll = mconcat [ts1,ts2,ts3,ts4]

median :: (Real a) => [a] -> Double
median xs | odd $ length xs = realToFrac $ xs !! (lowerIdx + 1)
          | otherwise = (lower + upper) / 2
  where (lowerIdx, upperOffset) = length xs `divMod` 2
        lower = realToFrac $ xs !! lowerIdx
        upper = realToFrac $ xs !! (lowerIdx + upperOffset)

medianTS :: (Real a) => TS a -> Maybe Double
medianTS (TS _ values) = justApply median values

mean :: (Real a) => [a] -> Double
mean xs = total/count
  where total = (realToFrac . sum) xs
        count = (realToFrac . length) xs

meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ values) = justApply mean values

type CompareFunc a = a -> a -> a
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare func = newFunc
  where newFunc (i1, Nothing) (_, Nothing) = (i1, Nothing)
        newFunc (_, Nothing) (i, val) = (i, val)
        newFunc (i, val) (_, Nothing) = (i, val)
        newFunc (i1, Just val1) (i2, Just val2) = if func val1 val2 == val1
                                                  then (i1, Just val1)
                                                  else (i2, Just val2)

compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS _ (TS [] []) = Nothing
compareTS func (TS times values) = if all (== Nothing) values
                                   then Nothing
                                   else Just best
  where pairs = zip times values
        best = foldl (makeTSCompare func) (0, Nothing) pairs

minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max

pairApp :: Num a => (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
pairApp f = (<*>) . (pure f <*>)

diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair = pairApp (-)

divPair :: Integral a => Maybe a -> Maybe a -> Maybe a
divPair = pairApp div

diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = mempty
diffTS (TS times values) = TS times (Nothing : diffValues)
  where shiftValues = tail values
        diffValues = zipWith diffPair shiftValues values

justApply :: (Real a) => ([a] -> Double) -> [Maybe a] -> Maybe Double
justApply f = (f <$>) . sequence

moving :: (Real a) => ([Maybe a] -> Maybe Double) -> [Maybe a] -> Int -> [Maybe Double]
moving f vals n
  | length nextVals == n = f nextVals : moving f restVals n
  | otherwise = []
  where nextVals = take n vals
        restVals = tail vals

movingTS :: (Real a) => ([a] -> Double) -> TS a -> Int -> TS Double
movingTS _ (TS [] []) _ = mempty
movingTS f (TS times values) n = TS times smoothedValues
  where transformedVals = moving (justApply f) values n
        nothings = replicate (n `div` 2) Nothing
        smoothedValues = mconcat [nothings, transformedVals, nothings]

movingAverageTS :: (Real a) => TS a -> Int -> TS Double
movingAverageTS = movingTS mean

movingMedianTS :: (Real a) => TS a -> Int -> TS Double
movingMedianTS = movingTS median