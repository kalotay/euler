import Ratio
import qualified Data.Map as Map

primes = 2: oddprimes
oddprimes = 3: sieve oddprimes 3 0
sieve (p:ps) x k
        = [n | n <- [x+2,x+4..p*p-2]
                , and [rem n p/=0 | p <- take k oddprimes]]
        ++ sieve ps (p*p) (k+1)

hmmdecode :: [Char] -> [Map.Map Int Rational] -> 
             [Map.Map Char Rational] -> [Rational] -> [Rational]
hmmdecode (c:cs) trans emis prior = 
        zipWith (\x y -> x * Map.findWithDefault 0 c y) posterior emis
        where posterior
                | cs == [] = prior
                | otherwise = [Map.foldWithKey
                        (\k p acc -> acc + p * updated !! (k - 1)) 0 x | x <- trans]
              updated = hmmdecode cs trans emis prior

croakProb :: Bool -> Map.Map Char Rational
croakProb True = Map.fromList [('P', 2 % 3), ('N', 1 % 3)]
croakProb False = Map.fromList [('P', 1 % 3), ('N', 2 % 3)]

trans_prob :: Int -> [Map.Map Int Rational]
trans_prob n = [neighbo x n | x <- [1..n]]
        where neighbo 1 _ = Map.fromList [(2, 1 % 1)]
              neighbo x n
                | x == n = Map.fromList [((n - 1), (1 % 1))]
                | otherwise = Map.fromList [((x - 1), (1 % 2)), ((x + 1), (1 % 2))]


used_primes n = takeWhile (<=n) primes

obs_prob n = [croakProb $ elem x $ used_primes x | x <- [1..n]]

seed n = take n $ repeat (1 % 500)

obs_sequence = ['P', 'P', 'P', 'P', 'N', 'N', 'P', 'P', 'P', 'N', 'P', 'P', 'N', 'P', 'N']

result = 199740353 % 29386561536000
