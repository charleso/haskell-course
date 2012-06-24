
import Debug.Trace
-- import Control.Parallel (par, pseq)
-- import Control.Parallel.Strategies

tests = [test_p1, test_p1', test_p2, test_p3, test_p3', test_p4, test_p5, test_p6, test_p7, test_p10]

{-
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
The sum of these multiples is 23.
Find the sum of all the multiples of 3 or 5 below 1000.
-}

p1 = p1' 1000
p1' = sum . p1''
p1'' n = filter (mods [3, 5]) $ [1..(n - 1)]
    where mods y x = any ((0 ==) . mod x) y

test_p1 = p1' 10 == 23
test_p1' = p1'' 10 == [3, 5, 6, 9]

{-
Each new term in the Fibonacci sequence is generated by adding the previous two terms. 
By starting with 1 and 2, the first 10 terms will be:

1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
-}

p2 = p2' 4000000
p2' n = sum . filter even . takeWhile (n >=) $ fib
fib = fib' 1 2
    where fib' a b = a : b : fib'' a b
          fib'' a b = let x = a + b in x : fib'' b x
          
test_p2 = take 10 fib == [1, 2, 3, 5, 8, 13, 21, 34, 55, 89]

{-
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
-}

p3 = p3' 600851475143
p3' = last . p3''
p3'' n = p3x n (tail . prime $ n)
    where p3x 1 a = []
          p3x _ [] = []
          p3x n' (h:t) = if m == 0 then h : p3x d t else p3x n' t
            where (d, m) = divMod n' h

prime n = prime' [1..n]
prime' = filter (\x -> not . any (\y -> x `mod` y == 0) $ [2..(x `div` 2)])
-- prime' = map (\(a, _) -> a) . filter (\(_, b) -> b) . parMap rseq (\x -> (x, not . any (\y -> x `mod` y == 0) $ [2..(x `div` 2)]))

test_p3 = p3' 13195 == 29
test_p3' = p3'' 13195 == [5, 7, 13, 29]

{-
A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 99.

Find the largest palindrome made from the product of two 3-digit numbers.
-}

p4 = p4' 100 999
p4' x y = foldl max 0 combs
    where combs = [a*b | a <- nums, b <- nums, is_pal (a*b)]
          nums = [x..y]

is_pal a = let b = show a in b == reverse b

test_p4 = p4' 10 99 == 9009

{-
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
-}

p5 = p5' 20
p5' b = head p5''
    where p5'' = filter (\x -> all (\y -> x `mod` y == 0) $ [2..b]) [b, (b*2)..]

-- I didn't work this out myself :(
p5_cheat = foldl lcm 1 [1..20]

test_p5 = p5' 10 == 2520

{-
The sum of the squares of the first ten natural numbers is,

1^2 + 2^2 + ... + 10^2 = 385
The square of the sum of the first ten natural numbers is,

(1 + 2 + ... + 10)^2 = 552 = 3025
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 - 385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
-}

p6 = p6' 100
p6' n = s2 - s1 
    where s1 = sum . map (^ 2) $ [1..n]
          s2 = (sum [1..n]) ^ 2

test_p6 = p6' 10 == 2640

{-
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10001st prime number?
-}

p7 = p7' 10001
p7' n = (prime' $ [1..]) !! n

test_p7 = p7' 6 == 13

{-
Find the greatest product of five consecutive digits in the 1000-digit number.
-}

p8_num = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

p8 = maximum . p8' $ (show p8_num)
p8' (_:_:_:_:_:[]) = []
p8' n = p8_prod (take 5 $ n) : p8' (tail n)

p8_prod = product . map (read . (:[]))

{-
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a^2 + b^2 = c^2
For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
-}

{-
a^2 + b^2 = c^2
a + b + c = 1000
=> c = 1000 - a - b
=> (1000 - a - b)^2 = a^2 + b^2
=> (1000 - (a + b))^2 = ...
=> 1000^2 - 2000(a + b) + (a + b)^2 = ...
=> 1000^2 - 2000a - 2000b + a^2 + 2ab + b^2 = a^2 + b^2
=> 1000^2 - 2000a - 2000b + 2ab = 0
=> 1000^2 = 2000a + 2000b - 2ab
=> 1000^2 = 2(1000a + 1000b - ab)
=> 500000 = 1000a + 1000b - ab
=> 500000 = 1000(a + b) - ab
-}

-- p9 = (200,375,425,1000)
p9 = take 1 . filter (\(a, b, c, x) -> x == 1000) $ p9' 50
p9' max = [(a, b, c, x) | a <- [1..max], b <- [1..max], c <- [1..max], a < b && b < c, a^2 + b^2 == c^2, let x = a + b + c]

p9_2 = p9_2' 500
p9_2' max = let (a, b) = head [(a, b) | a <- [1..max], b <- [1..max], a < b, 1000 * (a + b) - (a * b) == 500000]
           in (a, b, 1000 - a - b)

{-
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
-}

p10 = p10' 2000000
p10' = sum . tail . prime
    where prime n = prime' $ [1..n]
          prime' = filter (\x -> not . any (\y -> trace (show x) x `mod` y == 0) $ [2..(f' x)])
          f' = round . sqrt . fromIntegral
test_p10 = p10' 10 == 17

p10_brute = p10 == 142913828922

{-
In the 20 x 20 grid below, four numbers along a diagonal line have been marked in red.

The product of these numbers is 26 x 63 x 78 x 14 = 1788696.

What is the greatest product of four adjacent numbers in any direction (up, down, left, right, or diagonally) in the 20 x 20 grid?
-}

p11 = maximum . map product $ p11_every
p11_verbose = foldl (\a@(_, m1) b@(_, m2) -> if m1 < m2 then b else a) ([], 0) $ zip p11_every (map product p11_every)
p11_every = concat . map (p11_get p11_grid) $ [0..p11_full]

p11_num = 4
p11_size = 20
p11_full = p11_size * p11_size

p11_get grid pos = dmap (grid !!) . filter (all bounds) . dmap (pos +) $ p11_trans
    where dmap f = map (map f)
          bounds x = x >= 0 && x < p11_full

p11_trans = [hor (+), hor (-), ver (+), ver (-), diag (+) (+), diag (+) (-) {-, diag (-) (+), diag (-) (-)-}]
    where hor = str 1
          ver = str p11_size
          str a b = take p11_num (iterate (flip b a) 0)
          diag h v = zipWith (+) (hor h) (ver v)

p11_diag grid pos = pos

p11_grid = [08,02,22,97,38,15,00,40,00,75,04,05,07,78,52,12,50,77,91,08,
            49,49,99,40,17,81,18,57,60,87,17,40,98,43,69,48,04,56,62,00,
            81,49,31,73,55,79,14,29,93,71,40,67,53,88,30,03,49,13,36,65,
            52,70,95,23,04,60,11,42,69,24,68,56,01,32,56,71,37,02,36,91,
            22,31,16,71,51,67,63,89,41,92,36,54,22,40,40,28,66,33,13,80,
            24,47,32,60,99,03,45,02,44,75,33,53,78,36,84,20,35,17,12,50,
            32,98,81,28,64,23,67,10,26,38,40,67,59,54,70,66,18,38,64,70,
            67,26,20,68,02,62,12,20,95,63,94,39,63,08,40,91,66,49,94,21,
            24,55,58,05,66,73,99,26,97,17,78,78,96,83,14,88,34,89,63,72,
            21,36,23,09,75,00,76,44,20,45,35,14,00,61,33,97,34,31,33,95,
            78,17,53,28,22,75,31,67,15,94,03,80,04,62,16,14,09,53,56,92,
            16,39,05,42,96,35,31,47,55,58,88,24,00,17,54,24,36,29,85,57,
            86,56,00,48,35,71,89,07,05,44,44,37,44,60,21,58,51,54,17,58,
            19,80,81,68,05,94,47,69,28,73,92,13,86,52,17,77,04,89,55,40,
            04,52,08,83,97,35,99,16,07,97,57,32,16,26,26,79,33,27,98,66,
            88,36,68,87,57,62,20,72,03,46,33,67,46,55,12,32,63,93,53,69,
            04,42,16,73,38,25,39,11,24,94,72,18,08,46,29,32,40,62,76,36,
            20,69,36,41,72,30,23,88,34,62,99,69,82,67,59,85,74,04,36,16,
            20,73,35,29,78,31,90,01,74,31,49,71,48,86,81,16,23,57,05,54,
            01,70,54,71,83,51,54,69,16,92,33,48,61,43,52,01,89,19,67,48]