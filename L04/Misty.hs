module L04.Misty where

import L01.Optional
import L01.Validation
import L02.List
import L03.Parser


class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a -- unit
  -- Exercise 4
  -- Relative Difficulty: 3
  -- (use banana and unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' f = banana (unicorn . f)

-- Exercise 5
-- Relative Difficulty: 2
instance Misty List where
  banana = flatMap
  unicorn x = x :| Nil

-- Exercise 6
-- Relative Difficulty: 2
instance Misty Optional where
  banana = flip bindOptional
  unicorn = Full

-- Exercise 7
-- Relative Difficulty: 3
instance Misty Parser where
  banana = flip bindParser
  unicorn = valueParser

-- Exercise 8
-- Relative Difficulty: 2
jellybean :: Misty m => m (m a) -> m a
jellybean = banana id

-- Exercise 9
-- Relative Difficulty: 3
sausage :: Misty m => [m a] -> m [a]
sausage = foldr (lemon2 (:)) (unicorn [])

-- Exercise 10
-- Relative Difficulty: 3
moppy :: Misty m => (a -> m b) -> [a] -> m [b]
moppy f xs = sausage (foldr (\x ys -> f x : ys) [] xs)

-- Exercise 11
-- Relative Difficulty: 4
rockstar :: Misty m => Int -> m a -> m [a]
rockstar n m = sausage (replicate n m)

-- Exercise 12
-- Relative Difficulty: 9
filtering  :: Misty m => (a -> m Bool) -> [a] -> m [a]
--filtering = error "todo"
filtering _ []    = unicorn []
filtering f (h:t) = banana (\b -> furry' (\xs -> if b then h:xs else xs) (filtering f t)) (f h)

-- Exercise 13
-- Relative Difficulty: 10
apple :: Misty m => m (a -> b) -> m a -> m b
-- apple f a = banana (\f' -> furry' (\a' -> f' a') a) f
apple = lemon2 ($) -- or lemon2 id

-- Exercise 14
-- Relative Difficulty: 6
-- (bonus: use apple + furry') -- furry' :: (a -> b) -> m a -> m b
lemon2 :: Misty m => (a -> b -> c) -> m a -> m b -> m c
--lemon2 f m1 m2 = banana (\v1 -> banana (\v2 -> unicorn (f v1 v2)) m2) m1
lemon2 f a = apple (furry' f a)

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use apple + lemon2)
lemon3 :: Misty m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
--lemon3 f m1 m2 m3 = banana (\v1 -> banana (\v2 -> banana (\v3 -> unicorn (f v1 v2 v3)) m3) m2) m1
lemon3 f a b = apple ((lemon2 f a) b)

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + lemon3)
lemon4 :: Misty m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
--lemon4 f m1 m2 m3 m4 = banana (\v1 -> banana (\v2 -> banana (\v3 -> banana (\v4 -> unicorn (f v1 v2 v3 v4)) m4) m3) m2) m1
lemon4 f a b c = apple ((lemon3 f a b) c)

--
-- SUPPORT LIBRARIES --
--

instance Misty [] where
  banana = concatMap
  unicorn = return
