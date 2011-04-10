factorial :: Integer -> Integer
factorial n = product [1..n]

binom :: Integer -> Integer -> Integer
binom n k = factorial n `div` (factorial k * factorial (n - k))

routes :: Integer -> Integer
routes n = binom (2 * n) n
