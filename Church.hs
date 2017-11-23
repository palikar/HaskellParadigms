module Church where



type Church t = (t -> t) -> t -> t



int2church ::Integer -> Church Integer
int2church n = (\x -> (\y -> n))

church2int :: Church Integer -> Integer
church2int church = church (\x -> 0) 0


church :: Integer -> Church t
church 0 = \y -> \x -> x
church n = \y -> \x -> y ( church ( n - 1 ) y x )
 
unchurch :: Church Integer -> Integer
unchurch n = n (\x -> x + 1 ) 0

