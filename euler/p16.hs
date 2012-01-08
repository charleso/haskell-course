{-
2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?
-}

p16 = p16' 1000
p16' n = sum . map (read . (:[])) $ show (2 ^ n)

test_p16 = p16' 15 == 26
