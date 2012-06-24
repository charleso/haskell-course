{-
If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
-}

import Data.Char
import Data.List

test = [p17' 5 == 19, wordLength 342 == 23, wordLength 115 == 20]

p17 = p17' 1000
p17' i = sum . map wordLength $ [1..i]

wordLength = length . filter (' ' /=) . p'

p' = p False
p a i | i == 0                       = ""
      | i < 20                       = and ++ n i
      | i < 100                      = and ++ more False
      | otherwise                    = more True
      where (x : xs) = show i
            start = word (length xs) (digitToInt x)
            rest = flip p (read xs)
            more a = start ++ " " ++ rest a
            and = if a then "and " else ""

word i x
    | i == 0 = n x
    | i == 1 = d (x * 10)
    | otherwise = n x ++ " " ++ c (10 ^ i)

n 0 = ""
n 1 = "one"
n 2 = "two"
n 3 = "three"
n 4 = "four"
n 5 = "five"
n 6 = "six"
n 7 = "seven"
n 8 = "eight"
n 9 = "nine"
n 10 = "ten"
n 11 = "eleven"
n 12 = "twelve"
n 13 = "thirteen"
n 14 = "fourteen"
n 15 = "fifteen"
n 16 = "sixteen"
n 17 = "seventeen"
n 18 = "eighteen"
n 19 = "nineteen"

d 20 = "twenty"
d 30 = "thirty"
d 40 = "forty"
d 50 = "fifty"
d 60 = "sixty"
d 70 = "seventy"
d 80 = "eighty"
d 90 = "ninety"

c 100 = "hundred"
c 1000 = "thousand"
c i = error ("Invalid " ++ show i)
