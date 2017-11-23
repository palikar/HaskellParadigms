module Hirsch where
import Data.List




atLeastElements list n =  n <= length (filter (\x -> x >= n) list)


hIndexCorrect op list
  |op list > (length list) = False
  |otherwise = atLeastElements list n && not (atLeastElements list (n+1))
  where n = op list


hIndex list = check sorted 0 
  where sorted = reverse (sort list)
        check [] num = 0
        check (x:xs) num = if x <= num then num else check xs (num + 1)


validate =  hIndexCorrect hIndex [1,2,3,4,6]
