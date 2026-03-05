-- I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza

-- - két szám összegét, különbségét, szorzatát, hányadosát, osztási maradékát,
osszeg :: (Num a) => a -> a -> a
osszeg a b = a + b

osszeg2 a b = (+) a b

kulonbseg :: Double -> Double -> Double
kulonbseg a b = a - b

kulonbseg2 a b = (-) a b

szorzat :: Int -> Int -> Int
szorzat a b = a * b

szorzat2 a b = (*) a b

hanyados :: (Fractional a) => a -> a -> a
hanyados a b = a / b --(valos szamokra)

hanyados2 :: (Integral a) => a -> a -> a
hanyados2 a b = a `div` b --(egesz szamokra)

hanyados3 :: (Integral a) => a -> a -> a
hanyados3 a b = div a b


osztmar a b = mod a b

osztmar2 a b = a `mod` b


-- - egy első fokú egyenlet gyökét,
-- a*x + b = 0 -> a,b -> x = (-b) / a
elsoF a b = (-b) / a


-- - egy szám abszulút értékét,
abszolut n = if n < 0 then -n else n

abszolut2 n
    | n < 0 = -n
    | otherwise = n


-- - egy szám előjelét,
elojel n = if n < 0 then "negativ" else if n > 0 then "pozitiv" else "nulla"
elojel2 n
    | n < 0 = "negativ"
    | n > 0 = "pozitiv"
    | otherwise = "nulla"


-- - két argumentuma közül a maximumot,
max1 a b = if a > b then a else b

max2 a b
  | a > b = a
  | otherwise = b


  -- - két argumentuma közül a minimumot,
min1 a b = if a > b then a else b

min2 a b
  | a < b = a
  | otherwise = b


-- - egy másodfokú egyenlet gyökeit,
-- a*(x**2) + b*x + c = 0 -> a,b,c
-- delta = b**2 - 4*a*c
-- gy1 = (-b + sqrt delta) / (2*a) 
-- gy2 = (-b - sqrt delta) / (2*a) 
masodF a b c = if delta < 0 then error "komplex szamok" else (gy1, gy2)
    where 
        delta = b**2 - 4*a*c
        gy1 = (-b + sqrt delta) / (2 * a)
        gy2 = (-b - sqrt delta) / (2 * a) 

masodF2 a b c
    | delta < 0 = error "komplex szamok"
    | otherwise = (gy1, gy2)
    where 
        delta = b ** 2 - 4 * a * c
        gy1 = (-b + sqrt delta) / (2 * a)
        gy2 = (-b - sqrt delta) / (2 * a) 

masodF3 a b c
    | delta < 0 = error "komplex szamok"
    | delta == 0 = [gy1]
    | otherwise = [gy1, gy2]
    where 
        delta = b ** 2 - 4 * a * c
        gy1 = (-b + sqrt delta) / (2 * a)
        gy2 = (-b - sqrt delta) / (2 * a) 


-- - hogy két elempár értékei "majdnem" megegyeznek-e: akkor térít vissza True értéket a függvény, ha a két pár ugyanazokat az értékeket tartalmazza függetlenül az elemek sorrendjétől.
--   Például: $$(6, 7)$$ egyenlő $$(7,6)$$-al, de $$(6, 7)$$ nem egyenlő $$(4, 7)$$-el.
elempar :: Eq a => (a, a) -> (a, a) -> Bool
elempar ep1 ep2 = if (a == c && b == d) || (a == d && b == c) then True else False
    where
        (a, b) = ep1
        (c, d) = ep2

elempar2 :: Eq a => (a, a) -> (a, a) -> Bool
elempar2 ep1 ep2 = (a == c && b == d) || (a == d && b == c)
    where
        (a, b) = ep1
        (c, d) = ep2
        

-- - az n szám faktoriálisát (3 módszer),
-- 5! = 1*2*3*4*5
fakt1 0 = 1
fakt1 n = n * fakt1 (n - 1)

fakt2 n
    | n < 0 = error "negativ szam"
    | n == 0 = 1
    | otherwise = n * fakt2 (n - 1)


-- pelda meghivas: fakt3 1 5
fakt3 res n
    | n < 0 = -1
    | n == 0 = res
    | otherwise = fakt3 (n * res) (n - 1)


-- - az x szám n-ik hatványát, ha a kitevő pozitív szám (3 módszer).


-- II. Könyvtárfüggvények használata nélkül, illetve halmazkifejezéseket alkalmazva, definiáljuk azt a függvényt, amely meghatározza:

-- - az első n természetes szám negyzetgyökét,
negyzetgyokN n = [ sqrt i | i <- [1 .. n]] 


-- - az első n négyzetszámot,
negyzetN :: (Enum a, Floating a) => a -> [a]
negyzetN n = [i * i | i <- [1 .. n]]


-- - az első n természetes szám köbét,
kobN n = [i ^ 3 | i <- [1 .. n]]


-- - az első n olyan természetes számot, amelyben nem szerepelnek a négyzetszámok,
nemNegyzetN n = [i | i <- [1 .. n], (sqrt i * sqrt i) /= i]


-- - x hatványait adott n-ig,
xHatvanyN x n = [x ^ i | i <- [1 .. n]]


-- - egy szám páros osztóinak listáját,
osztokN n = [i | i <- [1 .. n], n `mod` i == 0, mod i 2 == 0]

osztokN2 n = [i | i <- [2, 4 .. n], mod n i == 0]


-- - n-ig a prímszámok listáját,
osztok n = [i | i <- [1 .. n], mod n i == 0] -- adott szam osszes osztoja

primszam n = osztok n == [1, n] -- primszam e

primszamokN n = [i | i <- [2 .. n], primszam i] -- primszam listaja

primszamokN2 n = [i | i <- [2 .. n], primszamL i]
    where 
        primszamL n = osztokL n == [1, n]
        osztokL n = [i | i <- [1 .. n], mod n i == 0]


-- - n-ig az összetett számok listáját, (ami nem primszam)
osszetettN n = [i | i <- [0 .. n], not (primszam i)]


-- - n-ig a páratlan összetett számok listáját,
paratlanOsszetettN n = [i | i <- [0 .. n], not (primszam i), mod i 2 /= 0]

paratlanOsszetettN2 n = [i | i <- [1, 3 .. n], not (primszam i)]


-- - az n-nél kisebb Pitágorászi számhármasokat,
pitagorasz n = [(a, b, c) | c <- [1 .. n], b <- [1 .. c], a <- [1 .. b], a ** 2 + b ** 2 == c ** 2]


-- - a következő listát: $$[(\texttt{a},0), (\texttt{b},1),\ldots, (\texttt{z}, 25)]$$,
betuSzam = zip ['a' .. 'z'] [0 .. 25] -- zip = tupple ertekbe teszi


-- - a következő listát: $$[(0, 5), (1, 4), (2, 3), (3, 2), (4, 1), (5, 0)]$$, majd általánosítsuk a feladatot.
szamok1 = zip [0 .. 10] [5, 4 .. 0]

szamok2 n = [(i, n - i) | i <- [0 .. n]]

szamok3 n = zip [0 .. n] [n, n - 1 .. 0]


-- - azt a listát, ami felváltva tartalmaz True és False értékeket.
tfLs n = take n ls -- take = adott mennyiseget vesz ki a listabol
    where
        ls = [True, False] ++ ls -- rekurzivan hozzafuz a ++ -> vegtelen lista


main :: IO()
main = do
    putStrLn "x hatvany n"
    print (xHatvanyN 5 3)
    putStrLn "paros osztok 48"
    print (osztokN 48)
    putStrLn "n-ig a paratlan osszetett szamok listajat"
    print (paratlanOsszetettN 100)
    putStrLn ("az n-nel kisebb Pitagoraszi szamharmasokat " ++ show (pitagorasz 100))
    print betuSzam
    print szamok1
    print (szamok2 10)
    print (szamok3 10)
    putStrLn ("5 True, False: " ++ show (tfLs 5))


-- **Megoldott feladatok:**

-- - Határozzuk meg egy szám osztóinak listáját:

--   ```haskell
--   osztok :: Int -> [ Int ]
--   osztok n = [ i | i <- [1..n] , n `mod` i ==0]

--   > osztok 100
--   ```

-- - Határozzuk meg a következő listát: $$[(\texttt{a},0), (\texttt{b},1), \ldots, (\texttt{z}, 25)]$$:

--   ```haskell
--   import Data.Char
--   lista = [(chr(i + 97), i) | i<-[0..25]]

--   lista_ = zip ['a'..'z'] [1..26]
--   ```
