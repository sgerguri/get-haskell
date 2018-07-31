module Lesson27 where

newtype Box a = Box a deriving Show

instance Functor Box where
  fmap f (Box x) = Box $ f x

morePresents :: Int -> Box a -> Box [a]
morePresents n (Box x) = Box $ replicate n x

myBox :: Box Int
myBox = Box 1

wrap :: a -> Box a
wrap = Box

unwrap :: Box a -> a
unwrap (Box x) = x
