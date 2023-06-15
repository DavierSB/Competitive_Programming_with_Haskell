https://dmoj.ca/problem/ccc15j5
import Data.Array

mem :: Array (Int, Int) Integer
mem = array ((1, 1), (250, 250)) $
                [((i, 1), 1) | i <- [1..250]]++
                [((k, n), mem!(k - 1, n - 1) + (if k - n >= n then mem!(k - n, n) else 0)) | k <- [1..250], n <- [2..k]]

main :: IO()
main = do {input1 <- getLine;
          input2 <- getLine;
        putStr (show (mem!(read input1 :: Int, read input2 :: Int)))}