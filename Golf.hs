import Data.Array
solve (x:_:z) = let final = read x :: Int
                    clubs = [read e :: Int | e <- z]
                    time = array (0, final)
                                 ([(0, 0)]++[(i, get_time i clubs) | i <- [1..final]])
                    get_time :: Int -> [Int] -> Int
                    get_time d [] = final + 1
                    get_time d (c : cs) | c > d = get_time d cs
                                        | (c == d) = 1
                                        | c < d = min (time!(d - c) + 1) (get_time d cs)
                in if (time!final) <= final then "Roberta wins in " ++ (show time!final) ++ " strokes."
                                            else "Roberta acknowledges defeat."

main :: IO()
main = interact $ solve.lines