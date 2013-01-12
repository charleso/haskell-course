{-
To load run 'ghci' and then ':l pinball.hs'
You can then interact with the top-level methods

One of the hardest things to remember about Haskell is that it (mostly) reads right to left, which is not what we're used to. eg. 

print . process' . parse $ context

is the same as:

print (process' (parse context))

Which then makes the order more obviously - parse is called first, then process' with the result, which is then printed.
-}



main = do
	context <- readFile "pinball.txt"
	print . process' . parse $ context

{-
The only place in the code where I _need_ a type signature

Try running 'parse' in ghci:

parse "1 2\n3 4"
> [[1, 2], [3, 4]]

-}

parse :: String -> [[Int]]
parse = map (map read . words) . lines


{-
This is the guts of the program. foldr1 'folds' over a list and calls a function with each successive row and the previous result:

Here is an example of just using the (+) function over a list of numbers:

foldr1 (+) [1, 2, 3] 
> [(1 + 2), 3]
> 3 + 3
> 6

We use our 'tilt' function from below to sum up the maximum value for each 'combination'

fold1 (tilt (+) max3) [[50, 10, 17], [19, 99, 16], [14, 30, 10]]
> [[50 + 99, 10 + 99, 16 + 99], [14, 30, 10]]
> [149 + 30, 109 + 30, 115 + 30]
> [179, 139, 145]

We then use 'maximum' to get the largest value

maximum [179, 139, 145]
> 179

-}

-- process' :: [[Int]] -> Int 
process' = maximum . foldr1 (tilt 0 (+) maximum)

{-
So tilt is just the logic for taking two rows and combining each element of the top row
with the three possible elements in the bottom row.

To see that try running the following:

tilt 0 (,) id [1, 4, 5] [3, 9, 2]
> [ (1, [0,3,9]) , (4, [3,9,2]) , (5, [9,2,0]) ]

So each element of the first row is combined with the relevant values on the next
-}

-- tilt :: a1 -> (a -> b -> c) -> ([a1] -> b) -> [a] -> [a1] -> [c]
tilt e f g y x' = zipWith f y (zipWith3 (\a b c -> g [a, b, c]) x (tail x) (tail . tail $ x))
    where x = e : x' ++ [e]


------------------------------------------


{-
This is similar to the Python version (without caching) and runs until the heat-death of the universe
-}

-- process :: (Num c, Ord c) => [[c]] -> c
process main_list = maximum . map (\x -> foobar x 0) $ [0..(size - 1)]
    where size = length main_list
          foobar x y = if x < 0 || x == size || y == size then 0 else (main_list !! y !! x) + (foobar2 x y)
          foobar2 x y = maximum (map (\i -> (foobar (x + i) (y + 1))) [-1, 0, 1])
