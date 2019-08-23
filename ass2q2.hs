data PolyTree a = Leaf a | Node (PolyTree a) (PolyTree a) deriving(Show,Eq,Read)

height      :: PolyTree a -> Int
height (Leaf _)    = 1
height (Node tl tr) = 1 + max (height tl) (height tr)


leafNumber :: PolyTree a -> Int
leafNumber (Leaf a) = 1
leafNumber (Node l r) = leafNumber l + leafNumber r


treeToList :: PolyTree a -> [a]
treeToList (Leaf a) = [a]
treeToList (Node l r) = treeToList l ++ treeToList r


labelTree :: PolyTree a -> PolyTree Int
labelTree x = labelTree' x 0

labelTree' :: PolyTree (a,Int) -> Int -> PolyTree Int
labelTree' (Leaf a) n = Leaf (n + 1 + a)
labelTree' (Node l r) n = Node (labelTree' l n) (labelTree' r n)

--flatTree
--zipTree
--unzipTree

