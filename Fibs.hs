module Fibs where

fibs = 0:1:(zipWith (+) (tail fibs) fibs) 






myIterate f a b = a:b:myIterate_ f (b) (f a b)
  where myIterate_ f a b = b:myIterate_ f b (f a b)
          
fibs_ = myIterate (\x x1 -> x+x1) 0 1
