{-- Useful functions for lists
								 null, take, drop, cycle, repeat, replicate -}



count :: Int -> [Int] -> Int
count _ [] = 0 
count ele (x:xs) = if (x == ele) then 1 + count ele xs
      			    else count ele xs


remove :: Int -> [Int] -> [Int]
remove ele xs = if(null xs) 
	then []
	else if (xs!!0 == ele)
	 then remove ele (drop 1 xs)
      else ([] ++ [head xs]) ++ remove ele (drop 1 xs)


sublist :: Int -> Int -> [Int] -> [Int]
sublist l r xs = if(l<=r) then xs!!l : sublist (l+1) r xs
                  else [] 


reversed :: [Int] -> [Int]
reversed xs = if(null xs)
	then []
	else reversed(drop 1 xs) ++ [head xs]


indexOf :: Int -> [Int] -> Int 
indexOf ele xs = if(null xs) 
	then -1
	else if (xs!!0 == ele)
	 then itr + 1
      else indexOf ele (itr+1) (drop 1 xs)

merge :: [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = [x] ++ [y] ++ merge xs ys


intersperse :: a -> [a] -> [a]
intersperse x (y:[]) = [y]
intersperse x (y:ys) = [y] ++ [x] ++ intersperse x ys

splitAt :: Int -> [a] -> ([a],[a])
splitAt x xs = (splitAtF x xs 1 (length xs),splitAtS x xs 1 (length xs))

splitAtF :: Int -> [a] -> Int -> Int -> [a]  
splitAtF p xs q len = if(p<1) then []
	                   else if(q>len) then []
				        else if(p==q) then [head xs]
                         else [head xs]++splitAtF p (tail xs) (q+1) len

splitAtS :: Int -> [a] -> Int -> Int -> [a]
splitAtS p xs q len = if(p<1) then xs
					   else if(q>len) then []
				  	    else if(p>=q) then splitAtS p (tail xs) (q+1) len
                         else [head xs]++splitAtS p (tail xs) (q+1) len




inits :: [a] -> [[a]] 
inits xs = reverse (inits' xs)

inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' xs = xs:inits'(init xs)


tails :: [a] -> [[a]]
tails [] = [[]]
tails xs = xs:tails (tail xs)

insert :: a -> [a] -> [[a]]
insert p xs = insert' p xs []

insert' :: a -> [a] -> [a] -> [[a]]
insert' p [] ys = [ys++[p]]
insert' p xs ys = (ys++[p]++xs):insert' p (tail xs) (ys++[head xs])