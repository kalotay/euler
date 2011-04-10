import Ratio

jump :: (Integer, Rational) -> [(Integer, Rational)]
jump (1, p) = [(2, p)]
jump (500, p) = [(499, p)]
jump (i, p) = [(pred i, p / 2), (succ i, p / 2)]

jumps :: [(Integer, Rational)] -> [(Integer, Rational)] -> [(Integer, Rational)]
jumps [] y = y
jumps (x:xs) [] = jumps xs (jump x)
jumps (x:xs) y = jumps xs (merge y (jump x))
        where merge y z
                | not $ or (map (\x -> elem x y) z) = y ++ z
                | otherwise = []

primes = 2: oddprimes
oddprimes = 3: sieve oddprimes 3 0
sieve (p:ps) x k
        = [n | n <- [x+2,x+4..p*p-2]
                , and [rem n p/=0 | p <- take k oddprimes]]
        ++ sieve ps (p*p) (k+1)

croak_prob :: Integer -> (Integer, Rational)
croak_prob n
        | elem n prime_list = (n, 2 % 3)
        | otherwise = (n, 1 % 3)
        where prime_list = takeWhile (<=n) primes
