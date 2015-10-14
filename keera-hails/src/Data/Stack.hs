module Data.Stack where

type Stack a = [a]

pop :: Stack a -> (a, Stack a)
pop (x:xs) = (x,xs)

push :: a -> Stack a -> Stack a
push = (:)

empty :: Stack a
empty = []