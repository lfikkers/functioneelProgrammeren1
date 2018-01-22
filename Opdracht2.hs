import Data.Char

euclid::Int->Int->Int
euclid x y = if y == 0 then x else euclid y (mod x y)

egcd :: Int -> Int -> (Int,Int,Int)
egcd 0 b = (b, 0, 1)
egcd a b = let (g, s, t) = egcd (b `mod` a) a
           in (g, t - (b `div` a) * s, s)

egcdZ :: Int -> Int -> (Int,Int,Int)
egcdZ a b
    | t < 0 = (g, s, t + a*b)
    | otherwise = (g, s, t)
    where (g, s, t) = egcd a b

rsaencrypt :: (Int,Int)->Int->Int
rsaencrypt (e,m) x = (x^e) `mod` m

rsadecrypt :: (Int,Int)->Int->Int
rsadecrypt (d,m) x = (x^d) `mod` m

encrypt :: Char -> Int
encrypt x = rsaencrypt (7,22523) (ord x)

decrypt :: Int -> Char
decrypt x = chr((rsadecrypt (6343,22523) x))

--Alice encrypt het met haar private key en de public key van bob
--Bob decrypt het met zijn private key en de public key van alice
--zo kan het alleen door elk ander uitgelezen worden

-- p = 101 q = 223
-- M = 22523
-- e = 7 d = 6343