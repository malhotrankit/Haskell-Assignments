data Tree a = Node a [Tree a]

{--maxBranching :: Tree a -> Int
maxBranching (Node _ xs) = max (length xs) (maxBranching' 0 xs)

maxBranching' :: Int -> [Tree a] -> [Tree a] -> Int
maxBranching' big [] = big
maxBranching' big (x:ys) = maxBranching x -}

maxBranching :: Tree a -> Int
maxBranching (Node _ xs) = length xs