module Treepartition where



data Tree a = Leaf | Node (Tree a) a (Tree a)

deleteMin :: Tree a -> (a, Tree a)
deleteMin (Node Leaf val right) = (val, right)
deleteMin (Node left val right) = (fst pair, (Node (snd pair) val right))
  where pair = deleteMin (left)


merge :: Tree a -> Tree a -> Tree a
merge Leaf t2 = t2
merge t1 Leaf = t1
merge (t1) (t2)
  = Node t1 (fst pair) (snd pair)
  where pair = deleteMin t2


 
partition :: (a -> Bool) -> Tree a -> (Tree a, Tree a)

partition fun (Node Leaf val Leaf) = if fun val then (Node Leaf val Leaf, Leaf) else (Leaf, Node Leaf val Leaf)

partition fun (Node left val Leaf) =
  if fun val
  then  (Node (fst pair_1) val Leaf,snd pair_1)
  else (fst pair_1, Node (snd pair_1) val Leaf)
  where
    pair_1 = partition (fun) (left)

partition fun (Node Leaf val right) =
  if fun val
  then  (Node Leaf val (fst pair_1), snd pair_1)
  else (fst pair_1, Node Leaf val (snd pair_1))
  where
    pair_1 = partition (fun) (right)

  
partition fun (Node left val right) =
  if fun val
  then  (merge (Node (fst pair_1) val Leaf) (fst pair_2) ,merge (snd pair_1)(snd pair_2) )
  else  (merge (fst pair_1) (fst pair_2) ,merge (Node (snd pair_1) val Leaf ) (snd pair_2) )
  where
    pair_1 = partition (fun) (left)
    pair_2 = partition (fun) (right)

tr = Node (Node Leaf 3 (Node Leaf 4 Leaf)) 5 (Node (Node Leaf 6 Leaf) 7 (Node Leaf 8 Leaf))
tr2 = Node (Node Leaf 8 (Node Leaf 9 Leaf)) 10 (Node (Node Leaf 60 Leaf) 70 (Node Leaf 80 Leaf))


