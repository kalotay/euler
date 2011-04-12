sum_count :: [Integer] -> Integer -> Integer
sum_count _ 0 = 1
sum_count poss amount = 
        sum [sum_count poss (amount - x) | x <- poss, x <= amount]
