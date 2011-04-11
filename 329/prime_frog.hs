import Ratio
import qualified Data.Map as Map

primes = 2: oddprimes
oddprimes = 3: sieve oddprimes 3 0
sieve (p:ps) x k
        = [n | n <- [x+2,x+4..p*p-2]
                , and [rem n p/=0 | p <- take k oddprimes]]
        ++ sieve ps (p*p) (k+1)

type MapInt = Map.Map Integer

hmmdecode :: [Char] -> MapInt (MapInt Rational) 
             -> MapInt (Map.Map Char Rational) -> MapInt Rational
             -> MapInt Rational
hmmdecode [] _ _ p = p
hmmdecode (c:cs) trans emis pr_prob = hmmdecode cs trans emis (t_prob po_prob)
        where t_prob x = Map.map (loc_t_prob po_prob) trans
              loc_t_prob y z = Map.fold
                               (\x acc -> acc + fst x * snd x) 0
                               (inter y z)
              po_prob = Map.map
                        (\x -> (Map.findWithDefault 0 c (fst x)) * snd x)
                        (inter emis pr_prob)
              inter a b = Map.intersectionWith (\x y -> (x, y)) a b

croakProb :: Bool -> Map.Map Char Rational
croakProb True = Map.fromList [('P', 2 % 3), ('N', 1 % 3)]
croakProb False = Map.fromList [('P', 1 % 3), ('N', 2 % 3)]

genSquares = Map.fromList [(x, neighbo x) | x <- [1..500]]
        where neighbo 1 = Map.fromList [(2, 1 % 1)]
              neighbo 500 = Map.fromList [(499, 1 % 1)]
              neighbo x = 
                Map.fromList [(x - 1, 1 % 2), (x + 1, 1 % 2)]

seq = ['P', 'P', 'P', 'P', 'N', 'N', 'P', 'P', 'P', 'N', 'P', 'P', 'N', 'P', 'N']

result = 199364971 % 29386561536000
