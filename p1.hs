import Data.List

problem1 :: (Eq a, Enum a, Num a) => a -> a -> a -> a
problem1 x y limit = sum . nub $ [x, x*2 .. limit - 1] ++ [y, y*2 .. limit - 1]