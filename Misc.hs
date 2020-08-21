module Misc where

-- inclusive
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

dropLast :: [a] -> [a]
dropLast l = take ((length l) - 1) l
