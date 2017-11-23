module Collatz where


collatz value = iterate fun value
  where
    fun a
      |mod a 2 == 0 = div a 2
      |otherwise = 3*a + 1


num a = numHelp 0 (collatz a)
  where
    numHelp cnt list = if head list == 1 then cnt else numHelp (cnt+1) (tail list)

maxNum a b = maxNumHelp a b (0,0)
  where
    maxNumHelp a b current
      |a == b = current
      |res > (snd current) = maxNumHelp (a+1) b (a, res)
      |otherwise = maxNumHelp (a+1) b current
      where res = num a
