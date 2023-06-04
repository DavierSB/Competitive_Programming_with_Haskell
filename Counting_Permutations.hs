import Prelude hiding (lookup)
import Data.Map
import Data.Maybe

main :: IO ()
main = do
    line <- getLine
    let (line1, line2) = splitline line
        n = read line1 :: Int
        k = read line2 :: Int
    putStr (show (fst (cont_Perm n k empty)))

modul :: Integer
modul = 10^9 + 7

foul :: Integer
foul = -(modul + 1)

splitline :: String -> (String, String)
splitline (x : y : z) | y == ' ' = ([x], z)
                      | otherwise = let (f, s) = splitline(y : z)
                                     in (f ++ [x], s)

easy_calc :: Int -> Int -> Map (Int, Int) Integer -> Integer
easy_calc n k dict | cond = sumar (fromJust c1) (fromJust c2) n k dict
                   | otherwise = foul
                   where c1 = lookup (n - 1, k) dict
                         c2 = lookup (n, k - 1) dict
                         cond = ((isJust c1) && (isJust c2))

sumar :: Integer -> Integer -> Int -> Int -> Map (Int, Int) Integer -> Integer
sumar a b n k dict = mod ((mod (a + b - c) modul) + modul) modul
                     where c = if k >= n then fromJust (lookup (n - 1, k - n) dict) else 0

cont_Perm :: Int -> Int -> Map (Int, Int) Integer -> (Integer, Map (Int, Int) Integer)
cont_Perm 2 _ dict = (1, dict)
cont_Perm _ 0 dict = (1, dict)
cont_Perm n k dict | (value /= foul) = (value, dict)
                   | otherwise = let (c3, dict_A) = if k >= n then cont_Perm (n - 1) (k - n) dict else (0, dict)
                                     (c2, dict_B) = cont_Perm n (k - 1) dict_A
                                     (c1, dict_C) = cont_Perm (n - 1) k dict_B
                                     ans = mod (c1 + c2 - c3) modul
                                     new_dict = insert (n, k) ans dict
                                   in (ans, new_dict)
                     where value = easy_calc n k dict