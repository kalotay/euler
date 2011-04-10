import List
import Maybe

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

result = sum (filter (even) (take (fromJust (findIndex (> 4000000) fibs)) fibs))
