https://dmoj.ca/problem/dmopc21c5p3
import Data.Array
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

aux :: Int -> Int -> Int
aux p t = mod t p

primos :: Array Int Int
primos = array (1, 168)(zip [1..168] primes)

primes :: [Int]
primes = 2 : take 167 (sieve primes [3..])
  where
    sieve (p:ps) xs =
      let (h,t) = span (< p*p) xs
      in  h ++ sieve ps (filter ((/=0).(aux p)) t)

mem :: Int -> Int -> IntSet -> Int -> IntSet
mem x limit dict p | x == 1 = mem (x+1) limit (IntSet.insert 1 dict) p
                   | x == limit = dict
                   | (primos)!p >= x = mem (x + 1) limit (IntSet.insert x dict) 1
                   | not (IntSet.member (x - ((primos)!p)) dict) = mem x limit dict (p + 1)
                   | otherwise = mem (x + 1) limit dict 1

permutacion :: [Int] -> String
permutacion (x : []) = (show x)
permutacion (x : xs) = (show x)++[' ']++(permutacion xs)

machine :: IntSet
machine = mem 1 1000 IntSet.empty 1

solve :: IntSet ->String -> String
solve dict input | x == 2 = "-1"
                 | not(IntSet.member x dict) = permutacion [1..x]
                 | not(IntSet.member (x - 1) dict) = permutacion ([1..(x-2)]++[(x), (x-1)])
                 | otherwise = permutacion ([1..(x-3)]++[(x), (x-2), (x-1)])
                 where x = read input :: Int

main :: IO()
main = interact $ unlines.(map (solve machine)).tail.lines