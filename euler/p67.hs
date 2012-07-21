module P67 where

import P18
import Text.Regex

parse = map (\s -> read s :: Integer) . concatMap (splitRegex $ mkRegex " ") . lines

p67 = readFile "triangle.txt" >>= print . p18' . parse

