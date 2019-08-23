data IntTree = Leaf Int | Node IntTree IntTree deriving(Show,Read,Eq)

sumTree :: IntTree -> Int
sumTree (Leaf n) = n
sumTree (Node l r) = sumTree l + sumTree r

mirrorTree :: IntTree -> IntTree
mirrorTree (Leaf n) = Leaf n
mirrorTree (Node l r) = Node (mirrorTree r) (mirrorTree l)

label :: IntTree -> [Int]
label (Leaf n) = [n]
label (Node l r) = label l ++ label r

