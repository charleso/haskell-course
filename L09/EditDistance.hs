module L09.EditDistance
(
  editDistance
) where

import Data.Array
import Prelude hiding (any, minimum)
import Data.Foldable

editDistance ::
  Eq a =>
  [a]
  -> [a]
  -> Int
editDistance x y =
  let (t, i, j) = table x y
  in t ! (i, j)

-- do not export

table ::
  Eq a =>
  [a]
  -> [a]
  -> (Array (Int, Int) Int, Int, Int)
table xs ys  =
  let m      = length xs
      n      = length ys
      k i s  = (1,i) `array` zip [1..] s
      x      = k m xs
      y      = k n ys

      t      = bounds `array` [(z, distance z) | z <- range bounds]
      bounds = ((0,0),(m,n))

      distance (0,j) = j
      distance (i,0) = i
      distance (i,j) =
        let track = [(1,0,1),(0,1,1),(1,1, if x ! i == y ! j then 0 else 1)]
        in minimum . fmap (\(p, q, n) -> t ! (i-p,j-q) + n) $ track
  in (t, m, n)

