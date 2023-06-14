https://dmoj.ca/problem/primes2
main :: IO()
main = do
    line <- getLine
    let (line1, line2) = splitline line
        a = read line1 :: Integer
        b = read line2 :: Integer
    mostrar (primos a b [a..b] primes)

primes :: [Integer]
primes = 2 : take 3410 (sieve primes [3..])
  where
    sieve (p:ps) xs =
      let (h,t) = span (< p*p) xs
      in  h ++ sieve ps (filter ((/=0).(aux p)) t)

aux :: Integer -> Integer -> Integer
aux p t = mod t p

primos :: Integer -> Integer -> [Integer] -> [Integer] -> [Integer] 
primos a b suspects [] = suspects
primos a b suspects (p : ps) | b <= 31623 = [x | x <- (p : ps), x >= a, x <= b]
                             | a <= 31623 = (primos a 31623 [a..31623] (p : ps))++(primos 31624 b [31624..b] (p : ps))
                             | otherwise = primos a b [x | x <- suspects, mod x p /= 0] ps

mostrar :: [Integer] -> IO()
mostrar [] = putStr ""
mostrar (l : ls) = do putStr ((show l)++['\n'])
                      mostrar ls

splitline :: String -> (String, String)
splitline (x : y : z) | y == ' ' = ([x], z)
                      | otherwise = let (f, s) = splitline(y : z)
                                     in ([x] ++ f, s)