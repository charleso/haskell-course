module P20 where
import Data.Char

p20 = p20' 100
p20' = sum . map digitToInt . show . factorial 
factorial = product . enumFromTo 1
