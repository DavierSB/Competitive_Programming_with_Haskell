import Data.Array
import Text.Printf

main :: IO()
main = do
    {
        line1 <- getLine;
        line2 <- getLine;
        let a = read line1 :: Integer
            b = read line2 :: Integer
        in mostrar (procesar isCompound 0 primes a b) 0 a b
    }

isCompound :: Array Int Bool
isCompound = array (0, 5000000)([(i, False) | i <- [0..500000]])

procesar :: Array Int Bool -> Int -> [Integer] -> Integer -> Integer -> Array Int Bool
procesar sosp pos primos a b | (toInteger pos) + a > b = sosp
                             | sosp!pos = procesar sosp (pos + 1) primos a b
                             | otherwise = let n = (toInteger(pos) + a)
                                               root = sqrtInt n 
                                               (p, prm) = find_divisor n root primos
                                       in procesar (cribar p pos sosp) (pos + 1) prm a b

find_divisor :: Integer -> Integer -> [Integer] -> (Integer, [Integer])
find_divisor n root (p : ps) | p > root = (n, (eliminar n (p : ps)))
                             | (mod n p == 0) = (p, ps)
                             | otherwise = let (d, prm) = find_divisor n root ps
                                            in (d, [p]++prm)

cribar :: Integer -> Int -> Array Int Bool -> Array Int Bool
cribar p pos sosp = accumArray (||) False (0, 5000000) (assoc_list p pos)

assoc_list :: Integer -> Int -> [(Int, Bool)]
assoc_list p pos | p + (toInteger pos) > 5000000 = []
                 | otherwise = [((fromInteger p) + pos, True)]++(assoc_list p (pos + (fromInteger p))) 


--Auxiliares
eliminar :: Integer -> [Integer] -> [Integer]
eliminar x (p : ps) | x == p = ps
                    | otherwise = eliminar x ps

sqrtInt :: Integral a => a -> a
sqrtInt 0 = 0
sqrtInt 1 = 1
sqrtInt n = head $ dropWhile (not . isRoot) iters
    where twopows = iterate (^2) 2
          (lowerRoot, lowerN) = last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
          newtonStep x = div (x + div n x) 2
          iters = iterate newtonStep (sqrtInt (div n lowerN) * lowerRoot)
          isRoot r = (r^2) <= n && n < ((r+1)^2)

primes :: [Integer]
primes = 2 : take 3410 (sieve primes [3..])
  where
    sieve (p:ps) xs =
      let (h,t) = span (< p*p) xs
      in  h ++ sieve ps (filter ((/=0).(aux p)) t)

aux :: Integer -> Integer -> Integer
aux p t = mod t p

mostrar :: Array Int Bool -> Int -> Integer -> Integer -> IO()
mostrar sosp pos a b | (toInteger pos) > b = printf ""
                     | (sosp!pos == False) = do printf "%d" ((toInteger pos) + a)
                                                mostrar sosp (pos+1) a b
                     | otherwise = mostrar sosp (pos+1) a b 
                                      