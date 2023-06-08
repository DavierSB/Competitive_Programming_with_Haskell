main :: IO ()
main = interact $
    unlines.(map solve).lines

solve :: String -> String
solve "X" = ""
solve s = imprim (isMonkey False True s)

imprim :: Bool -> String
imprim True = "YES\n"
imprim _ = "NO\n"

--    B abierta    Iniciando
isMonkey :: Bool -> Bool -> String -> Bool
isMonkey openB inicio [] = not(inicio) && not(openB)
isMonkey _ inicio ('B' : tail) = inicio && (isMonkey True True tail)
isMonkey openB inicio ('A' : tail) = inicio && (isMonkey openB False tail)
isMonkey openB inicio ('N' : tail) = not(inicio) && (isMonkey openB True tail)
isMonkey openB inicio ('S' : tail) = not(inicio) && (isMonkey False False tail)