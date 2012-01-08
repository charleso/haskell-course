
import Debug.Trace

{-
The following iterative sequence is defined for the set of positive integers:

n -> n/2 (n is even)
n -> 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
-}


p14 = p14'' 999999
p14'' n = foldl (\a@(_, b) c@(_, d) -> if b >= d then a else trace (show c) c) (0, 0) . map (\(a, b) -> (a, length b)) . map (\x -> (x, p14' x)) $ enumFromThenTo n (n-2) 1

p14' n = 1 : (fullSeq n)
    where next n = if even n then n `div` 2 else 3 * n + 1
          fullSeq = takeWhile (/= 1) . iterate next

test_p14 = p14' 13 == [1,13,40,20,10,5,16,8,4,2]

p14_brute = p14 == (837799,525)
