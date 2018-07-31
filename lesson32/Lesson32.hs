module Lesson32 where

endDates :: [Int]
endDates = [31,28,31,30,31,30,31,31,30,31,30,31]

dates :: [[Int]]
dates = [ [1..endDate] | endDate <- endDates ]

datesDo :: [[Int]]
datesDo = do
  endDate <- endDates
  return [1..endDate]

datesMonad :: [[Int]]
datesMonad = endDates >>= (\endDate -> return [1..endDate])
