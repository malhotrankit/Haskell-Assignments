data BsTree a = Empty | Branch (BsTree a) a (BsTree a) deriving (Show,Read,Eq,Ord)



insert :: Ord a => a -> BsTree a -> BsTree a
insert x Empty = Branch Empty x Empty
insert x (Branch l y r) = if(x `compare` y == GT) then
							if(r `compare` Empty == EQ) then
								Branch l y (Branch (Empty) x (Empty))
								else
								 Branch l y (insert x r) 
						  else
						  	if(l `compare` Empty == EQ) then
						       Branch (Branch (Empty) x (Empty)) y r
						  	  else
						  	   Branch (insert x l) y r