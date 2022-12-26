{- | PriorityQueue

A simple priority queue.  Returns items from smallest to largest.

This implementation assumes that the Set type stores its contents in
a sorted list, and it's reasonably efficient to insert and delete
elements. 

NOTE: Because we use a set for storage, the queue cannot contain
two identical elements.
-}

module PriorityQueue (PriorityQueue, delete, empty, insert, null, peek, size) where

import Prelude hiding (null)
import qualified Data.Set as S (Set, delete, empty, insert, size, toList, null)

-- | PriorityQueue is a wrapper for a Set
newtype Ord a => PriorityQueue a = PriorityQueue (S.Set a)

-- | The empty PriorityQueue
empty :: Ord a => PriorityQueue a
empty = PriorityQueue S.empty

-- | The number of elements in a priority queue
size :: Ord a => PriorityQueue a -> Int
size (PriorityQueue s) = S.size s

-- | Inserts a new element into the queue
insert :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insert x (PriorityQueue s) = PriorityQueue (S.insert x s)

-- | Deletes the specified item from the queue
delete :: Ord a => a -> PriorityQueue a -> PriorityQueue a 
delete a (PriorityQueue s) = PriorityQueue (S.delete a s) 

-- | Returns true if-and-only-if the queue is empty.
null :: Ord a => PriorityQueue a -> Bool 
null (PriorityQueue s) = S.null s

-- | Returns the item at the head of the queue, without modifying the queue.
peek :: Ord a => PriorityQueue a -> Maybe a 
peek (PriorityQueue s)
 | S.null s = Nothing
 | otherwise = Just (head (S.toList s))
