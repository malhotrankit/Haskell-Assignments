data Op = Plus|Minus deriving(Show,Eq,Ord)
data Exp a = Lit a|Binary Op (Exp a) (Exp a) deriving(Show,Eq,Ord)



opList:: Exp a -> [Op]
opList (Lit x) = []
opList (Binary op x y) = [op] ++ opList x ++ opList y

