module Deque (Deque(..)) where

class Deque q where
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
