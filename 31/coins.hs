coin_count :: Integer -> [Integer] -> [Integer]
coin_count _ [] = []
coin_count 0 _ = []
coin_count i (x:xs)
        | i >= x = (++) [x] (coin_count ((-) i x) (x:xs))
        | otherwise = coin_count i xs

net_coin_count :: Integer -> [Integer] -> [[Integer]]
net_coin_count j y
        | j > z = (++) [z:zs] (net_coin_count (j - z) (z:zs))
        | otherwise = (++) [z:zs] (net_coin_count j zs)
        where (z:zs) = coin_count j y
              
