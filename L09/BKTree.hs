module L09.BKTree where

import L09.MetricSpace
import Data.Map (Map)
import qualified Data.Map as M
import Prelude hiding (any)
import Data.Foldable

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

withinDistance _ _ Leaf =
  []
withinDistance n a (Node z m) =
  let d = z <--> a
      k = undefined
  in if d <= k then z : k else k

