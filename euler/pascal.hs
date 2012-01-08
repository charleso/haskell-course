{-
Pascal's Triangle

      1
     1 1
    1 2 1
   1 3 3 1
  1 4 6 4 1
 1 5 10 10 5 1
1 6 15 20 15 6 1

-}

main = putStrLn . unlines . map show . map pascall $ [1..10]

pascall 1 = [1]
pascall n = map 
    (\a -> (prev !! a) + (prev !! (a + 1)))
    [0..n-1]
    where prev = 0 : pascall (n-1) ++ [0]

-- Not mine. A much clean way of doing this
-- http://news.ycombinator.com/item?id=3430911

step row = zipWith (+) (0:row) (row++[0])
triangle = iterate step [1]
main2 = take 10 triangle
