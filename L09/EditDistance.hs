module L09.EditDistance {-
(
  editDistance
) -} where

import Data.Array
import Prelude hiding (any, minimum)
import Data.Foldable

editDistance ::
  Eq a =>
  [a]
  -> [a]
  -> Int
editDistance x y =
  let t                    = table x y
      ((x0, y0), (x1, y1)) = bounds t
  in t ! (x1 - x0, y1 - y0)

data Edit a =
  Delete
  | Insert a
  | Subst a
  | Copy
  deriving (Eq, Show)

diff ::
  Eq a =>
  [a]
  -> [a]
  -> [Edit a]
diff a b =
  let diff _ _ 0 0 = []
      diff c d p q = let n = t ! (p, q)
                         n' = n - 1
                         o = t ! (p - 1, q - 1)
                         (g, h, i, j, k) = if n' == o
                                             then
                                               (Subst (head d), drop 1, drop 1, 1, 1)
                                             else
                                               if n' == t ! (p - 1, q)
                                                 then
                                                   (Delete, drop 1, id, 1, 0)
                                                 else
                                                   if n' == t ! (p, q - 1)
                                                     then
                                                       (Insert (head d), id, drop 1, 0, 1)
                                                     else
                                                       (Copy, drop 1, drop 1, 1, 1)
                     in g : diff (h c) (i d) (p - j) (q - k)
      t = table a b
      a' = reverse a
      b' = reverse b
      ((x0, y0), (x1, y1)) = bounds t
  in reverse $ diff a' b' (x1 - x0) (y1 - y0)

applyDiff ::
  [a]
  -> [Edit a]
  -> [a]
applyDiff =
  undefined


-- do not export

table ::
  Eq a =>
  [a]
  -> [a]
  -> Array (Int, Int) Int
table xs ys  =
  let m      = length xs
      n      = length ys
      k i s  = (1,i) `array` zip [1..] s
      x      = k m xs
      y      = k n ys

      t      = b `array` [(z, distance z) | z <- range b]
      b      = ((0,0),(m,n))

      distance (0,j) = j
      distance (i,0) = i
      distance (i,j) =
        let track = [(1,0,1),(0,1,1),(1,1, if x ! i == y ! j then 0 else 1)]
        in minimum . fmap (\(p, q, n) -> t ! (i-p,j-q) + n) $ track
  in t
