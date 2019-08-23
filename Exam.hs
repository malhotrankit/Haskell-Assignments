data T a = Empty| L a | B (T a) (T a)

count :: T a -> Int
count x = count' x 0

count' :: T a -> Int -> Int
count' (L a) _ = 1
count' (B l r) i = count' l i + count' r i


mapTree :: (a -> b) -> T a -> T b
mapTree f (L x) = L (f x)
mapTree f (B l r) = B (mapTree f l) (mapTree f r)


treeToList :: T a -> [a]
treeToList (L x) = [x]
treeToList (B l r) = treeToList l ++ treeToList r


foldTree :: (a -> b) -> (b -> b -> b) -> T a -> b
foldTree leaf branch (L x) = leaf x
foldTree leaf branch (B l r) = branch (foldTree leaf branch l) (foldTree leaf branch r) 

mapTreeF :: (a -> b) -> T a -> T b
mapTreeF f x = foldTree f  x