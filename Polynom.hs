module Polynom where

type Polynom = [Double]

add p1 p2 = zipWith (+) p1 p2

eval pol val = foldr (function) (last pol) (init pol)
  where
    function a res =  a + val*res 

  

deriv (p:px) = foldl function [] (px)
  where
    function cnt x = cnt ++ [newVal]
      where newVal = x * ((length cnt) + 1)





