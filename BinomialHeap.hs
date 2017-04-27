module BinomialHeap (BinomialHeap(..)) where

import Heap

data Tree a = Node Int a [Tree a]
data BinomialHeap a = BH [Tree a]

rank (Node r x c) = r
root (Node r x c) = x

link t1@(Node r x1 c1) t2@(Node _ x2 c2)
  | x1 <= x2  = Node (r + 1) x1 (t2 : c1)
  | otherwise = Node (r + 1) x2 (t1 : c2)

insertTree t [] = [t]
insertTree t ts@(t' : ts')
  | rank t < rank t' = t : ts
  | otherwise        = insertTree (link t t') ts'

merge ts1 [] = ts1
merge [] ts2 = ts2
merge ts1@(t1 : ts1') ts2@(t2 : ts2')
  | rank t1 < rank t2 = t1 : merge ts1' ts2
  | rank t2 < rank t1 = t2 : merge ts1' ts2
  | otherwise         = insertTree (link t1 t2) (mrg ts1' ts2')

removeMinTree [] = error "empty heap"
removeMinTree [t] = (t, [])
removeMinTree (t:ts)
  | root t < root t' = (t, ts)
  | otherwise        = (t', t : ts')
    where (t', ts') = removeMinTree ts

instance Heap Binomial Heap where
  empty = BH []
  isEmpty (BH ts) = null ts

  insert x (BH ts) = BH (insTree (Node 0 x []) ts)
  merge (BH ts1) (BH ts2) = BH (merge ts1 ts2)

  findMin (BH ts) = root t
    where (t, _) = removeMinTree ts

  deleteMin (BH ts) = BH (merge (reverse ts1) ts2)
    where (Node _ x ts1, ts2) = removeMinTree ts
