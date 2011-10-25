{-
Write a function that generates a text-based ruler. The function takes two parameters:

    Height
    Number of sections

Example: Ruler(6, 2)

Result:

|                               |                               |
|               |               |               |               |
|       |       |       |       |       |       |       |       |
|   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |
| | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
-}

import Data.List

main = draw 6 2

draw a b = putStrLn $ unlines $ ruler a b

ruler height sections = transpose
    $ map transpose' 
    $ map reverse 
    $ concat (replicate sections ([line height] ++ drawSection' height)) ++ [line height]

transpose' = replace '-' '|'
    where replace old new = map (\i -> if i == old then new else i)

drawSection' height = map (\s -> s ++ space (height - length s)) $ drawSection height

drawSection height = if height == 1 
    then ["-"]
    else inner ++ [line height] ++ inner
        where inner = drawSection' (height - 1)

line = line' '-'
space = line' ' '
line' c n = replicate n c
