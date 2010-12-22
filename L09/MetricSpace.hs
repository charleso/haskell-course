module L09.MetricSpace where

import L09.EditDistance

class MetricSpace a where
  (<-->) ::
    a
    -> a
    -> Int

instance Eq a => MetricSpace [a] where
  (<-->) =
    editDistance

