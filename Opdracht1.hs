faca::Int->Int
faca 0 = 1
faca x = x * faca (x-1)

facb::Int->Int
facb x
    | x == 0    = 1
    | x >= 1    = x * facb(x-1)

nulpuntena::Double->Double->Double->[Double]
nulpuntena 0 0 0 = 0 : []
nulpuntena a b c = 
    if (b^2 - 4*a*c) > 0
        then -b-(b^2 - 4*a*c)/2*a : -b+(b^2 - 4*a*c)/2*a : []
        else if (b^2 - 4*a*c) == 0
            then -b/2*a : []
            else error "Géén snijpunten" : []

nulpuntenb::Double->Double->Double->[Double]
nulpuntenb a b c
    | a == 0 && b == 0 && c == 0 = 0 : []
    | d == 0 =  -b/2*a : []
    | d > 0 = -b-d/2*a : -b+d/2*a : []
    | otherwise = error "Géén snijpunten" : []
    where d = b^2 - 4*a*c

dobbelsteena = [(x,y,z)|x<-[1..6],y<-[1..6],z<-[1..6],(x+y+z) `mod` 5 == 0]

dobbelsteenb n = [(x,y,z)|x<-[1..6],y<-[1..6],z<-[1..6],(x+y+z) `mod` n == 0]