import Data.Array
main :: IO()
main = do {input1 <- getLine;
           input2 <- getLine;
           input3 <- getLine;
        let (arr1, arr2, l1, l2) = get_arrays input1 input2 input3
            mem :: Array (Int, Int) Int
            mem = array((0,0), (l1, l2))
                       (
                        [((i, 0), 0) | i <- [0..l1]]++
                        [((0, i), 0) | i <- [1..l2]]++
                        [((i, j), get_value i j)| i <- [1..l1], j <- [1..l2]]
                       )
                       where get_value i j | (arr1!i == arr2!j) = 1 + mem!(i - 1, j - 1)
                                           | otherwise = max (mem!(i-1, j)) (mem!(i, j-1))
        in putStr (show (mem!(l1, l2)))}

get_arrays :: String -> String -> String -> (Array Int Int, Array Int Int, Int, Int)
get_arrays input1 input2 input3 = let (l1, l2) = splitLine input1
                                      arr1 = armar_array input2 l1
                                      arr2 = armar_array input3 l2
                                    in (arr1, arr2, l1, l2)

splitLine :: String -> (Int, Int)
splitLine input = let w = words input
                   in (read (w!!0) :: Int, read (w!!1) :: Int)

armar_array :: String -> Int -> Array Int Int
armar_array input l = let w = words input
                          leer x = read x :: Int
                       in array (1, l)(zip [1..l] (map leer w))
                        