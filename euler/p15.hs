{-
Starting in the top left corner of a 2x2 grid, there are 6 routes (without backtracking) to the bottom right corner.

+--+--+
|  |  |
+--+--+
|  |  |
+--+--+

+--+--+--+
|  |  |  |
+--+--+--+
|  |  |  |
+--+--+--+
|  |  |  |
+--+--+--+

2 = 6
3 = 20
4 = 70
5 = 252
6 = 924
7 = 3432
8 = 12870
9 = 48620

http://mathworld.wolfram.com/CentralBinomialCoefficient.html

How many routes are there through a 20x20 grid?
-}

p15_cheat n = (fact (2 * n)) `div` ((fact n) * (fact n))
    where fact n = foldl (*) 1 [2..n]

p15 = p15' 20
p15' n = move $ (0, 0)
    where move (x, y) | x == n && y == n = 1
                      | x == n + 1 = 0
                      | y == n + 1 = 0
                      | otherwise = move (x, y + 1) + move (x + 1, y)

test_p15 = p15' 2 == 6
