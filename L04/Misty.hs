module L04.Misty where

import L01.Optional
import L01.Validation
import L02.List
import L03.Parser

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 4
  -- Relative Difficulty: 3
  -- (use banana and unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' f =  banana (unicorn . f)

-- Exercise 5
-- Relative Difficulty: 2
instance Misty List where
  banana = error "todo"
  unicorn = flip (:|) Nil

-- Exercise 6
-- Relative Difficulty: 2
instance Misty Optional where
  banana = flip bindOptional
  unicorn = Full

-- Exercise 7
-- Relative Difficulty: 3
instance Misty Parser where
  banana = error "todo"
  unicorn = error "todo"

-- Exercise 8
-- Relative Difficulty: 2
jellybean :: Misty m => m (m a) -> m a
jellybean = banana id

-- Exercise 9
-- Relative Difficulty: 3
sausage :: Misty m => [m a] -> m [a]
--sausage = foldr (\x -> \xs -> lift2 (++) (furry' (\a -> a : []) x) xs) (unicorn [])
--sausage = foldr (\x -> lift2 (++) (furry' (\a -> a : []) x)) (unicorn [])
--sausage = foldr (lift2 (:)) (unicorn [])
sausage = foldr (\a b -> banana (flip furry' b . (:)) a ) (unicorn [])
--sausage = error "todo"


--sausage' = moppy (\a -> XXX) 

-- Exercise 10
-- Relative Difficulty: 3
moppy :: Misty m => (a -> m b) -> [a] -> m [b]
--moppy = error "todo"
moppy f a = sausage (unicorn undefined)

-- TODO Write sausage in terms of moppys

-- Exercise 11
-- Relative Difficulty: 4
rockstar :: Misty m => Int -> m a -> m [a]
rockstar = error "todo"

-- Exercise 12
-- Relative Difficulty: 9
filtering  :: Misty m => (a -> m Bool) -> [a] -> m [a]
filtering = error "todo"

-- Exercise 13
-- Relative Difficulty: 10
apple :: Misty m => m (a -> b) -> m a -> m b
--apple = lemon2 ($)
apple = lemon2 id
 
-- Exercise 14
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
lemon2 :: Misty m => (a -> b -> c) -> m a -> m b -> m c

--lift2 f a b = banana (\a' -> banana (\b' -> unicorn (f a' b')) b) a 
--lift2 f a b = banana (\a' -> banana (unicorn . f a') b) a 
--lift2 f a b = banana (\a' -> furry' (f a') b) a 
--lift2 f a b = banana (\a' -> flip furry' b (f a')) a 
lemon2 f a b = banana (flip furry' b . f) a 

-- TODO apple + life(m-1) = lift (M)

{-

lift0 = unit/unicorn/return
lift1 = fmap/furry
lift2 = ....

liftN = apple + lifeN-1
-}

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use apple + lemon2)
lemon3 :: Misty m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
--lemon3 :: Misty m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lemon3 f a b c =  apple undefined (lemon2 (\a' -> \b' -> f a' b' undefined) a b)

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + lemon3)
lemon4 :: Misty m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
lemon4 = error "todo"

--
-- SUPPORT LIBRARIES --
--

instance Misty [] where
  banana = concatMap
  unicorn = return
