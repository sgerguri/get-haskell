module Lesson13 where

cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc x = if x == maxBound
              then minBound
              else succ x
