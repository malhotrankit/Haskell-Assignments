import Data.List
import Data.Map 

diamond' :: Int -> Int -> IO()
diamond' n i 
		| n == 0 = return ()
		| n > i = putStr . unlines $ (row n i) ++ (row n (i+1))
		| n <= i =  putStr . unlines $ (row n i) ++ (row n (i-1))


row :: Int -> Int -> String
row 1 _ = " *"
row n i = space (n-i) ++ "*" ++ space (2*i-3) ++ "*" ++ space (n-i)


space :: Int -> String
space n = replicate n ' '
		


