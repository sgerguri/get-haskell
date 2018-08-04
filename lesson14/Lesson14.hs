module Lesson14 where

data Number = One | Two | Three deriving (Enum, Show)

instance Eq Number where
  (==) x y = fromEnum x == fromEnum y

instance Ord Number where
  (<=) x y = fromEnum x <= fromEnum y
