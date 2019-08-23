exponentiation :: Int -> Int -> Int
exponentiation _ 0 = 1
exponentiation b e = b * exponentiation b (e-1)


optExponentiation :: Int -> Int -> Int
optExponentiation _ 0 = 1
optExponentiation b e = if(even e)
	then optExponentiation b (div e 2) * optExponentiation b (div e 2)
	else b * optExponentiation b (div e 2) * optExponentiation b (div e 2)


main = do
	putStrLn "b to the power of e  using exponentiation is: "
	print(exponentiation 2 1)
	putStrLn "b to the power of e  using optExponentiation is: "
	print(optExponentiation 2 1)
