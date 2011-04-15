import Ratio
import qualified Data.Map as Map

primes = 2: oddprimes
oddprimes = 3: sieve oddprimes 3 0
sieve (p:ps) x k
        = [n | n <- [x+2,x+4..p*p-2]
                , and [rem n p/=0 | p <- take k oddprimes]]
        ++ sieve ps (p*p) (k+1)


hmmdecode :: [Char] -> [[Rational]] -> 
             [Map.Map Char Rational] -> [Rational] -> [Rational]
hmmdecode (c:cs) trans emis prior = 
        zipWith (\x y -> x * Map.findWithDefault 0 c y) posterior emis
        where posterior
                | length cs == 0 = prior
                | otherwise = [sum $ 
                                zipWith (*) t $ hmmdecode cs trans emis prior
                                | t <- trans]

croakProb :: Bool -> Map.Map Char Rational
croakProb True = Map.fromList [('P', 2 % 3), ('N', 1 % 3)]
croakProb False = Map.fromList [('P', 1 % 3), ('N', 2 % 3)]

genSquares n = [neighbo x n | x <- [1..n]]
        where neighbo x n = zeros (x - 2) ++ [1 % 2] ++ zeros 1 ++ 
                                [1 % 2] ++ zeros (n - x - 1)
              zeros m = take m (repeat (0 % 1))

sequence = ['P', 'P', 'P', 'P', 'N', 'N', 'P', 'P', 'P', 'N', 'P', 'P', 'N', 'P', 'N']

result = 201094913 % 29386561536000
