--https://dmoj.ca/problem/ccc20j4
main :: IO ()
main = do
    t <- getLine
    s <- getLine
    putStr (exe t s)

shifts :: String -> [String]
shifts (x:xs) = (x:xs) : shifts (xs ++ [x])

get_substrings :: String -> Int -> [String]
get_substrings (x:xs) long | length (x:xs) < long = []
                           | otherwise = let act = take long (x:xs)
                                             fut = get_substrings xs long
                                          in act : fut  

exe :: String -> String -> String
exe t s = let l = length s
              ps = take l (shifts s)
              subs = get_substrings t l
              cumplen = [x | x <- ps, elem x subs]
           in if length cumplen > 0 then "yes" else "no"