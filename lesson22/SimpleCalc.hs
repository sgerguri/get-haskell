{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module SimpleCalc where

import qualified Data.Char as C
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Prelude as P

data Expr a where
    N :: Num a => a -> Expr a
    (:+) :: Num a => Expr a -> Expr a -> Expr a
    (:*) :: Num a => Expr a -> Expr a -> Expr a


lineToEquation :: T.Text -> [T.Text]
lineToEquation = P.filter (not . T.null) . T.split C.isSpace . T.strip

parseEquation :: Num a => [T.Text] -> Expr a
parseEquation (l : op : r : []) = pl pop pr
    where pl = 
parseEquation (x : "+" : y : []) = N (decimal x) :+ N (decimal y)
parseEquation (x : "*" : y : []) = N (deimal x) :* N (decimal y)

-- createEquation :: Num a => [T.Text] -> a
-- createEquation (x : ":+:" : y) = (read x) + (read y)
-- createEquation (x : ":*:" : y) = (read x) * (read y)


-- main :: IO ()
-- main = do
--     equation <- T.lines <$> getContents
--     let eqParts = lineToEquation equation


    