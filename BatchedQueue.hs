module BatchedQueue where

import Prelude hiding (head, tail)
import Queue

data BatchedQueue a = BQ [a] [a]
  deriving (Show, Eq)

instance Queue BatchedQueue where
  empty = BQ [] []
  isEmpty (BQ f r) = null f

  snoc (BQ [] r) x = BQ [x] r
  snoc (BQ f  r) x = BQ f   (x:r)

  head (BQ [] _) = error "empty queue"
  head (BQ (x:xs) r) = x

  tail (BQ [] _) = error "empty queue"
  tail (BQ [x] r) = BQ (reverse r) []
  tail (BQ (x:xs) r) = BQ xs r
