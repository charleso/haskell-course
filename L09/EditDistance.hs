module L09.EditDistance where

import Data.Array
import qualified Data.Map as M
import Data.Map (Map)
import Prelude hiding (any, minimum)
import Data.Foldable

class MetricSpace a where
  (<-->) ::
    a
    -> a
    -> Int

instance Eq a => MetricSpace [a] where
  x <--> y =
    let (t, i, j) = table x y
    in t ! (i, j)

data BKTree a =
  Node a (Map Int (BKTree a))
  | Leaf

null ::
  BKTree a
  -> Bool
null Leaf =
  True
null (Node _ _) =
  False

size ::
  BKTree a
  -> Int
size Leaf =
  0
size (Node _ m) =
  M.size m

(.:.) ::
  MetricSpace a =>
  a
  -> BKTree a
  -> BKTree a
a .:. Leaf =
  Node a M.empty
a .:. Node z m =
  let d = z <--> a
  in Node z (M.alter (\zz -> Just $! case zz of
                                       Just w -> a .:. w
                                       Nothing -> Node a M.empty) d m) -- (pp (a .:.) d m)

asList ::
  BKTree a
  -> [a]
asList Leaf =
  []
asList (Node _ m) =
  fmap snd (M.toList m) >>= asList

(-?-) ::
  MetricSpace a =>
  a
  -> BKTree a
  -> Bool
_ -?- Leaf =
  False
a -?- Node z m =
  let d = z <--> a
  in d == 0 || any ((-?-) a) (d `M.lookup` m)


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
