module Lesson31 where
import qualified Data.Map as Map

type Pizza = (Double, Double)

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi*(size/2)^2

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2
                      then p1
                      else p2
                        where costP1 = costPerInch p1
                              costP2 = costPerInch p2

describePizza :: Pizza -> String
describePizza (size,cost) =
  concat ["The ", show size, " pizza is cheaper at ", show costSqInch, " per square inch"]
    where costSqInch = costPerInch (size,cost)


main :: IO ()
main =
  putStrLn "What is the size of pizza 1"
  >> getLine
  >>= (\size1 ->
    putStrLn "What is the cost of pizza 1"
    >> getLine
    >>= (\cost1 ->
      putStrLn "What is the size of pizza 2"
      >> getLine
      >>= (\size2 ->
        putStrLn "What is the cost of pizza 2"
        >> getLine
        >>= (\cost2 ->
          (\pizza1 ->
            (\pizza2 ->
              (\betterPizza -> putStrLn (describePizza betterPizza))
              $ comparePizzas pizza1 pizza2)
              (read size2, read cost2))
              (read size1, read cost1)))))

sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1,20.0),(2,15.0)]

costData :: Map.Map Int Double
costData = Map.fromList [(1,18.0),(2,16.0)]

maybeMain :: Maybe String
maybeMain =
  Map.lookup 1 sizeData
  >>= (\size1 ->
    Map.lookup 1 costData
    >>= (\cost1 ->
      Map.lookup 2 sizeData
      >>= (\size2 ->
        Map.lookup 2 costData
        >>= (\cost2 ->
          (\pizza1 ->
            (\pizza2 ->
              (\betterPizza ->
                return $ describePizza betterPizza)
              $ comparePizzas pizza1 pizza2)
            (size2, cost2))
          (size1, cost1)))))

listMain :: [String]
listMain = do
  size1 <- sizes1
  cost1 <- costs1
  size2 <- sizes2
  cost2 <- costs2
  let pizza1 = (size1,cost1)
  let pizza2 = (size2,cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)
    where sizes1 = [20.0,15.0]
          costs1 = [18.0,16.0]
          sizes2 = [11.0,17.0,18.0]
          costs2 = [10.0,11.0,12.0]

monadMain :: Monad m => m Double -> m Double -> m Double -> m Double -> m String
monadMain sizeM1 costM1 sizeM2 costM2 = do
  size1 <- sizeM1
  cost1 <- costM1
  size2 <- sizeM2
  cost2 <- costM2
  let pizza1 = (size1,cost1)
  let pizza2 = (size2,cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return $ describePizza betterPizza
