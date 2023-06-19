--https://dmoj.ca/problem/dmopc21c5p3
import Data.Array

aux :: Int -> Int -> Int
aux p t = mod t p

primos :: Array Int Int
primos = array (1, 9592)(zip [1..9592] primes)

primes :: [Int]
primes = 2 : take 9591 (sieve primes [3..])
  where
    sieve (p:ps) xs =
      let (h,t) = span (< p*p) xs
      in  h ++ sieve ps (filter ((/=0).(aux p)) t)

mem :: Array Int Bool
mem = array (1, 100000)
                      (
                        [(1, False)]++[(i, value i 1) | i <- [2..100000]]
                      )
                      where value x p | primos!p >= x = False
                                      | mem!(x - primos!p) == False = True
                                      | otherwise = value x (p + 1)

permutacion :: [Int] -> String
permutacion (x : []) = (show x)
permutacion (x : xs) = (show x)++[' ']++(permutacion xs)

solve :: String -> String
solve input = let x = read input :: Int
                  value n | n == 2 = "-1"
                          | mem!n = permutacion [1..n]
                          | mem!(n-1) = permutacion([1..(n-2)]++[n, n-1])
                          | otherwise = permutacion([1..(n-3)]++[n, n-2, n-1])
                in value x

main :: IO()
main = --interact $ unlines.(map solve).tail.lines
       mem!99000