module Sample exposing (..)

fac : Int -> Int
fac n = if n > 1
        then n * fac (n-1)
        else 1
