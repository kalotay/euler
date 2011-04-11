import Ratio
import qualified Data.Map as Map
import qualified Data.Maybe as Mb

primes = 2: oddprimes
oddprimes = 3: sieve oddprimes 3 0
sieve (p:ps) x k
        = [n | n <- [x+2,x+4..p*p-2]
                , and [rem n p/=0 | p <- take k oddprimes]]
        ++ sieve ps (p*p) (k+1)

data State = State { stateTransits :: Map.Map Integer Rational
                   , observeTransits :: Map.Map Char Rational
                   } deriving (Show)

calcProb :: [Char] -> Map.Map Integer (State, Rational) -> Rational
calcProb [] _ = 1
calcProb (c:cs) states = pc * calcProbs cs n_states
