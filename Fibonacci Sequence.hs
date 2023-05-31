-- https://dmoj.ca/problem/fibonacci
import Prelude hiding (lookup)
import Data.Map
import Data.Maybe
import Data.Bits
main :: IO ()
main = do
    input <- getLine
    let ordinary = take 1000 (fib_tonto 0 1)
        n = read input :: Integer
    putStr(show (fst(fast_fib ordinary empty n)))

fib_tonto :: Integer -> Integer -> [Integer]
fib_tonto a b = a : fib_tonto b (mod (b + a) 1000000007)

fast_fib :: [Integer] -> Map Integer Integer -> Integer -> (Integer, Map Integer Integer)
fast_fib firsts specials n | n < 1000 = (firsts!!(fromInteger n), specials)
                           | isJust value = (fromJust value, specials)
                           | otherwise = let k = if even n then shiftR n 1 else shiftR (n + 1) 1
                                             (f1, dict_temp) = fast_fib firsts specials k
                                             (f2, dict) = fast_fib firsts dict_temp (k - 1)
                                             mult x y = mod (x*y) 1000000007
                                             cuad x = mult x x
                                             fimpar = mod (cuad f1 + cuad f2) 1000000007
                                             fpar = mult f1 (mod ((mult 2 f2) + f1) 1000000007)
                                             ans = if odd n then fimpar else fpar
                                             dict_final = insert n ans dict
                                           in if odd n then (fimpar, dict_final) else (fpar, dict_final)
                            where value = lookup n specials