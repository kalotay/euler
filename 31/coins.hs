sum_count :: [Integer] -> Integer -> Integer
sum_count _ 0 = 1
sum_count [] _ = 0
sum_count [1] _ = 1
sum_count (x:xs) amount
        | x > amount = sum_count xs amount
        | otherwise = sum_count (x:xs) (amount - x) + sum_count xs amount 
