module BatchedDeque(BatchedDeque(..)) where

import Prelude hiding (head, tail)
import Deque
{-
empty   :: q a
isEmpty :: q a -> Bool

-- For working with front of queue
cons    :: a -> q a -> q a
head    :: q a -> a
tail    :: q a -> q a

-- For working with end of queue
snoc    :: q a -> a -> q a
last    :: q a -> a
init    :: q a -> q a
-}

data BatchedDeque a = BD [a] [a]

split (BD f r)
  | null f = BD ()
    where reversed x = reverse x

instance Deque BatchedDeque where
  -- Route r nodes to front when f is empty
  empty = BD [] []
  isEmpty (BD f r) = null f && null r

  cons x (BD f r) = BD (x:f) r

  head (BD []     r) = error "empty queue"
  head (BD (f:fs) r) = f

  tail (BD []     r) = error "empty queue"
  tail (BD [f]    r) = BD (reverse r) []
  tail (BD (f:fs) r) = BD fs r

  snoc (BD [] r) x = BD [x] r
  snoc (BD f  r) x = BD f (x:r)

  last (BD [] r) = error "empty queue"
  last (BD )
