divideByTen :: Float ->   Float
divideByTen x = x/10

divideByTen' :: Float -> Float
divideByTen' = (/10)

subtracter :: Int -> Int
subtracter x = x + (-4)

subtracter' :: Int -> Int
subtracter' = ( + (-4))

-- map function 
map' :: (a -> b) -> [a] -> [b]  
map' _ [] = []  
map' f (x:xs) = f x : map' f xs 

 -- filter function
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) = if(f x == True) then
                     x : filter' f xs
                   else
                   	 filter' f xs

-- takeWhile function
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) = if(f x == True) then
	                    x : takeWhile' f xs
	                  else []

head' :: [a] -> a
head' xs = foldl1 (\acc _ -> acc) xs

last' :: [a] -> a
last' xs = foldr1 (\_ acc -> acc) xs

{--
map using foldr (Run in online compiler)
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f xs = foldr ((:).f) [] xs -} --(\acc x -> acc ++ [f x])

{--
map using foldl (Run in online compiler)
mapFoldl::(a -> b) -> [a] -> [b]
mapFoldl f xs = foldl (\acc x -> acc ++ [f x]) [] xs
-}


-- elem using foldl
elemFoldl :: Eq a => a -> [a] -> Bool
elemFoldl y xs = foldl (\acc x -> if(x==y) then True else acc) False xs

-- elem using foldr
elemFoldr :: Eq a => a -> [a] -> Bool
elemFoldr y xs = or (foldr ((:).(y==)) [] xs)





-- Assingment2 Q4a 
data Tree a = Node a [Tree a] deriving Show

maxBranching :: Tree a -> Int
maxBranching (Node _ ts) = let localBranching = length ts in
                             max localBranching (maxBranchingOfSubtrees ts)

  where maxBranchingOfSubtrees :: [Tree a] -> Int
        maxBranchingOfSubtrees [] = 0
        maxBranchingOfSubtrees (x:xs) = max (maxBranching x) (maxBranchingOfSubtrees xs)


-- Assingnment 3 Q1
data SearchTree a = Empty | Branch (SearchTree a) a (SearchTree a) deriving Show

insert :: (a -> a -> Ordering) -> a -> SearchTree a -> SearchTree a
insert _   x Empty      = Branch Empty x Empty
insert cmp x (Branch l n r)
  | cmp x n == EQ       = Branch l n r
  | cmp x n == LT       = Branch (insert cmp x l) n r
  | otherwise           = Branch l n (insert cmp x r)

 
