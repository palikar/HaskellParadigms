fuk n = snd (foldl (\acc element -> ((fst acc) / element , (snd acc) + (fst acc) / element ) ) (1,1) [1..n]) 


pow1 b 0 = 1
pow1 b e = b * pow1 b (e-1)


pow2 b 0 = 1
pow2 b 1 = b
pow2 b e = b * pow2 (b*b) (e/2)


pow3 b e = pow3_help b e 1
   where pow3_help b e acc 
           |e > 1 = pow3_help (b)(e-1)(b*acc)
           |e == 1 = b*acc 
           |otherwise = 1


insert a [] = a : []
insert a (s:xs)
  |a <= s = (a:(s:xs))
  |null xs = [s,a]
  |otherwise = (s:insert a xs)



sort (x:xs) list = sort xs (insert x list) 
sort [] list = list

insertionSort list = sort list []



merge [] (b:bs) = (b:bs)
merge (a:bs) [] = (a:bs)
merge (a:as)(b:bs)
  | a <= b = (a: merge as (b:bs))
  | a > b = (b:merge (a:as) bs)


mergeSort list
  |n == 1 = list
  |otherwise = merge (mergeSort (take (div n 2) list)) (mergeSort (drop (n - (div n 2)) list))
  where n = length list
