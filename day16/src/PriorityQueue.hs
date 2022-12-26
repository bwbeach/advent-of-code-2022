module PriorityQueue (PriorityQueue, delete, empty, insert, null, peek, size) where

import Prelude hiding (null)

import qualified Data.Set as S (Set, delete, empty, insert, size, toList, null)

newtype Ord a => PriorityQueue a = PriorityQueue (S.Set a)

empty :: Ord a => PriorityQueue a
empty = PriorityQueue S.empty

size :: Ord a => PriorityQueue a -> Int
size (PriorityQueue s) = S.size s

insert :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insert x (PriorityQueue s) = PriorityQueue (S.insert x s)

null :: Ord a => PriorityQueue a -> Bool 
null (PriorityQueue s) = S.null s

peek :: Ord a => PriorityQueue a -> Maybe a 
peek (PriorityQueue s)
 | S.null s = Nothing
 | otherwise = Just (head (S.toList s))

delete :: Ord a => a -> PriorityQueue a -> PriorityQueue a 
delete a (PriorityQueue s) = PriorityQueue (S.delete a s) 
