module SomeModule where

f = head

r:: [Int] -> Int
r (x:xs) = x +10

g = f [f]

h = f