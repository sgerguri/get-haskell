sumSquareOrSquareSum x y = if sumSquare > squareSum
                           then sumSquare
                           else squareSum
  where sumSquare = x^2 + y^2
        squareSum = (x + y)^2

sumSquareOrSquareSum2 x y = (\sumSquare squareSum ->
                              if sumSquare > squareSum
                              then sumSquare
                              else squareSum) (x^2 + y^2) ((x+y)^2)

doubleDouble x = dubs*2
  where dubs = x*2

doubleDouble2 x = (\y -> y*2) (x * 2)

sumSquareOrSquareSum3 x y = let sumSquare = x^2 + y^2
                                squareSum = (x+y)^2
                                in
                                 if sumSquare > squareSum
                                 then sumSquare
                                 else squareSum

overwrite x = let x = 2
              in
               let x = 3
               in
                let x = 4
                in
                 x

overwrite2 x = (\x -> (\x -> (\x -> x) 4) 3) 2

counter x = (\x -> x + 1) ((\x -> x + 1) x)
