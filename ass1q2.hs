decToBinList :: Int -> [Bool]
decToBinList 1 = [True]
decToBinList n = if(mod n 2==0) then decToBinList (div n 2) ++ [False] 
				 else decToBinList (div n 2) ++ [True]

binToDec :: [Bool] -> Int
binToDec xs = binToDec' xs 0 (length xs)

binToDec' :: [Bool] -> Int -> Int -> Int
binToDec' [] _ _= 0
binToDec' (x:xs) i len = if(x == True) then
					   2^(len - i -1) + binToDec' xs (i+1) len
					  else
                        0 + binToDec' xs (i+1) len


data Binary = IHi | I Binary | O Binary deriving(Show)

decB :: Int -> Binary
decB 1 = IHi
decB n = if(mod n 2 == 0) then
		   O(decB (div n 2))
		  else
		    I(decB (div n 2))

binD :: Binary -> Int
binD x = binD' x 0

binD' :: Binary -> Int -> Int
binD' IHi i = 2^i
binD' (O p) i = 0 + binD' p (i+1)
binD' (I p) i = 2^ i + binD' p (i+1)
