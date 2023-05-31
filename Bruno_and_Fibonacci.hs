--https://dmoj.ca/problem/gfsspc1p4
main :: IO ()
main = do
    input <- getLine
    let fibs = take 16 (fib 1 2)
        n = read input :: Int
    cad <- getLine
    print (ans (check 1 cad fibs))

fib :: Int -> Int -> [Int]
fib a b = a : fib b (b + a)

ans :: Bool -> String
ans b = if b then "That's quite the observation!" else "Bruno, GO TO SLEEP"

check :: Int -> String -> [Int] -> Bool
check _ [] _ = True
check idx (cad_h : cad_t) (f : ft) | (idx == f) = ((cad_h == 'A') && (fut ft))
                                   | otherwise = if cad_h == 'A' then False else (fut (f : ft))
                                   where fut = check (idx + 1) cad_t