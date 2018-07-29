module Lesson17 where

data Color = Red
           | Yellow
           | Blue
           | Green
           | Purple
           | Orange
           | Brown
           | Clear
           deriving (Show, Eq)

instance Semigroup Color where
    (<>) Clear color = color
    (<>) color Clear = color
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red = Orange
    (<>) Red Yellow = Orange
    (<>) a b | a == b = a
             | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
             | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
             | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
             | otherwise = Brown

instance Monoid Color where
    mempty = Clear
    mappend = (<>)
    mconcat = foldr mappend mempty

newtype Events = Events [String]
newtype Probs = Probs [Double]
data PTable = PTable Events Probs

instance Semigroup Events where
    (<>) = combineEvents

instance Monoid Events where
    mempty = Events []
    mappend = (<>)

instance Semigroup Probs where
    (<>) = combineProbs

instance Monoid Probs where
    mempty = Probs [1.0]
    mappend = (<>)

instance Semigroup PTable where
  (<>) ptable1 (PTable (Events []) (Probs [])) = ptable1
  (<>) (PTable (Events []) (Probs [])) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where newEvents = e1 <> e2
          newProbs = p1 <> p2

instance Monoid PTable where
  mempty = PTable (Events []) (Probs [])
  mappend = (<>)

instance Show PTable where
  show (PTable (Events e) (Probs p)) = mconcat pairs
        where pairs = zipWith showPair e p

createPTable :: Events -> Probs -> PTable
createPTable (Events events) (Probs probs) = PTable (Events events) (Probs normalizedProbs)
    where totalProbs = sum probs
          normalizedProbs = map (\x -> x/totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event,"|",show prob,"\n"]

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
    where nToAdd = length l2
          repeatedL1 = map (take nToAdd . repeat) l1
          newL1 = mconcat repeatedL1
          cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents (Events e1) (Events e2) = Events $ cartCombine combiner e1 e2
  where combiner = (\x y -> mconcat [x,"-",y])

combineProbs :: Probs -> Probs -> Probs
combineProbs (Probs p1) (Probs p2) = Probs $ cartCombine (*) p1 p2
