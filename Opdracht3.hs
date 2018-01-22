import Data.List


--Opdracht 1

differentieer :: (Double->Double)->Double->Double->Double
differentieer f p x = (f (x+p) - f x) / p

integreer :: (Double->Double)->Double->Double->Double->Double
integreer f a b p = foldr (\l r -> (dx * (f (a + (l * dx)))) + r) 0 [0..p-1]
    where dx = (b-a)/p


--Opdracht 2

dubbelen :: Ord a=>[a]->[a]
dubbelen s = sort(nub((s) \\ (nub s)))

--Opdracht 3

stenen = [[a,b,c,d,e]|a<-s,b<-s,c<-s,d<-s,e<-s]
    where s = [1..6]

count::Integer->[Integer]->Integer
count c [] = 0
count c (x:xs)
    |c==x= 1 + (count c xs)
    |otherwise = count c xs
           
convert list = ([a,b,c,d,e,f],list)
    where
        a= count 1 list
        b= count 2 list
        c= count 3 list
        d= count 4 list
        e= count 5 list
        f= count 6 list
            
same::Integer->[[Integer]]->[[Integer]]
same x list = filter (elem x) $map fst (map convert list)
    
poker = (fromIntegral aantalpoker) / (fromIntegral (length stenen))
    where
    aantalpoker = length pokerlijst
        where
        pokerlijst = same 5 stenen
            
fourofakind = (fromIntegral aantalfour) / (fromIntegral (length stenen))
    where
    aantalfour = length fourlijst
        where
        fourlijst = same 4 stenen
           
fullhouse = (fromIntegral aantalfullhouse) / (fromIntegral (length stenen))
    where
    aantalfullhouse = length fullhouselijst
        where
        fullhouselijst = filter (elem 2) threelijst
            where
            threelijst = same 3 stenen
                
threeofakind = kansopdrie - fullhouse
    where
    kansopdrie = (fromIntegral aantaldrie) / (fromIntegral (length stenen))
        where
        aantaldrie = length drielijst
            where
            drielijst = same 3 stenen
  
twopair = (fromIntegral aantaltwopair) / (fromIntegral (length stenen))
    where
    aantaltwopair = length twopairlijst
        where
        twopairlijst = same 2 pairlijst
            where
            pairlijst = same 2 stenen
           
pair = twosame - twopair - fullhouse
    where
    twosame = (fromIntegral aantaltwosame) / (fromIntegral (length stenen))
        where
        aantaltwosame = length twosamelijst
            where
            twosamelijst = same 2 stenen
  
straight = (fromIntegral aantalstraight) / (fromIntegral (length stenen))
    where  
    aantalstraight = 240
    
bust = 1 - pair - twopair - threeofakind - fullhouse - fourofakind - poker - straight