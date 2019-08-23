avalanche :: Integer -> [Integer]
avalanche n = quicksort [3^i*5^j*7^k|i<-[0..n],j<-[0..n],k<-[0..n]]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]
