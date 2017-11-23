module Merge where



merge [] (b:bs) = (b:bs)
merge (a:bs) [] = (a:bs)
merge (a:as)(b:bs)
  | a <= b = (a: merge as (b:bs))
  | a > b = (b:merge (a:as) bs)


-- aus der Vorlesung
odds = 1 : map (+2) odds
oddPrimes (p : ps) = p : (oddPrimes [p_ | p_ <- ps, p_ `mod` p /= 0])
primes = 2 : oddPrimes (tail odds)

primepowers 0 = 1:[]
primepowers n = merge [num ^ n | num<-primes] (primepowers (n-1))   
