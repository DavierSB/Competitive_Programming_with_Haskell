import Prelude hiding (lookup)
import Data.Map
import Data.Maybe

main :: IO ()
main = do
    line <- getLine
    let (n, k) = splitline line
    putStr fst(cont_Perm n k empty)

modul :: Integer
modul = 10^9 + 7

foul :: Integer
foul = -(modul + 1)

splitline :: String -> (String, String)
splitline (x : y : z) | y == ' ' = ([x], z)
                      | otherwise = let (f, s) = splitline(y : z)
                                     in (f ++ [x], s)

easy_calc :: Int -> Int -> Map (Int, Int) Integer -> Integer
easy_calc n k dict | isJust sum = mod (fromJust sum) modul
                   | otherwise = foul
                   where c1 = lookup (n - 1, k) dict
                         c2 = lookup (n, k - 1) dict
                         c3 = if k >= n then lookup (n - 1, k - n) dict else 0
                         sum = c1 + c2 - c3 

cont_Perm :: Int -> Int -> Map (Int, Int) Integer -> (Integer, Map (Int, Int) Integer)
cont_Perm 2 _ _ = 1
cont_Perm _ 0 _ = 1
cont_Perm n k dict | (value /= foul) = (value, dict)
                   | otherwise = let (c3, dict_A) = cont_Perm (n - 1) (k - n) dict
                                     (c2, dict_B) = cont_Perm n (k - 1) dict_A
                                     (c1, dict_C) = cont_Perm (n - 1) k dict_B
                                     ans = mod (c1 + c2 - c3) modul
                                   in (ans, insert (n, k) ans dict_C) 
                     where value = easy_calc n k dict