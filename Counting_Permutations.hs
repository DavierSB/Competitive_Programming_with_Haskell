-- https://dmoj.ca/problem/mathp8
import Prelude hiding (lookup)
import Data.Array

modul = 10^9 + 7

main :: IO ()
main = do
    line <- getLine
    let (line1, line2) = splitline line
        n = read line1 :: Int
        k = read line2 :: Int
        mem = create_array (n+1) (k+1)
    putStr (show (mem!(n, k)))

splitline :: String -> (String, String)
splitline (x : y : z) | y == ' ' = ([x], z)
                      | otherwise = let (f, s) = splitline(y : z)
                                     in (f ++ [x], s)

create_array :: Int -> Int -> Array (Int,Int) Integer
create_array n k = a where
                   a = array ((2, 0), (n, k))
                         ([((2, i), 1) | i <- [0..k]]++
                          [((i, 0), 1) | i <- [3..n]]++
                          [((i, j), (mod (a!(i - 1, j) + a!(i, j - 1)) modul) - (if j >= i then a!(i - 1, j - i) else 0)) | (i, j) <- range ((3, 1), (n, k))]
                        )