https://dmoj.ca/problem/ccc11s3
main :: IO()
main = interact $ unlines.(map solve).tail.lines

solve :: String -> String
solve = traducir.solveA.splitLine

solveA :: (Int, Integer, Integer) -> Bool
solveA (0, x, y) = False
solveA (m, x, y) = let (a, b, c, d) = enfocar m x y
                       value | ((b == 2) && (a == 2)) = solveA (m - 1, c, d)
                             | (b == 1) = aux
                             | ((b == 0) && ((a == 1)||(a == 2)||(a == 3))) = True
                             | otherwise = False
                       aux | a == 2 = True
                           | ((a == 1) || (a == 3)) = solveA (m - 1, c, d)
                           | otherwise = False
                    in value

enfocar :: Int -> Integer -> Integer -> (Int, Int, Integer, Integer)
enfocar m x y = let unit = power 5 m
                    lesser_unit = power 5 (m - 1)
                    a = truncate ((dividir x unit) * 5)
                    b = truncate ((dividir y unit) * 5)
                    c = x - (a*lesser_unit)
                    d = y - (b*lesser_unit)
                in (fromInteger a, fromInteger b, c, d)

dividir :: Integer -> Integer -> Float
dividir x y = (fromInteger x) / (fromInteger y)

power :: Integer -> Int -> Integer
power b 1 = b
power b n = b*(power b (n - 1))

splitLine :: String -> (Int, Integer, Integer)
splitLine input = let palabras = words input
                  in (read (palabras!!0) :: Int, read (palabras!!1) :: Integer, read (palabras!!2) :: Integer)

traducir :: Bool -> String
traducir False = "empty"
traducir True = "crystal"