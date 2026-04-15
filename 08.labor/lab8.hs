import Data.Char 
import Data.List (group, sort, intercalate)
import System.Win32 (LOCALESIGNATURE(lsCsbSupported, lsCsbDefault), xBUTTON1, lOCALE_SDAYNAME4)
-- Parcialis gyakorlatok

-- 1. Egy [(String, Int)] típusú lista eleme egy városnevet és a megfelelő népesség
-- értéket tárolja. Írjunk egy Haskell függvényt, amely meghatározza, azokat a
-- városokat, amelyek népesség értéke egy adott n értéknél nagyobb. A kapott
-- városneveket ábécé sorrendbe rendezve külön sorba írjuk ki a képernyőre.
-- Például:
-- ● Bemenet: 150000 [("sepsiszentgyorgy", 54000), ("kolozsvár", 330000),
-- ("marosvasarhely", 130000), “temesvar", 310000), ("arad", 160000),
-- ("gyergyoszentmiklos", 18000), ("nagyvarad",196000)]
-- ● Kimenet:
-- A(z) 150000 nepesseg erteknel nagyobbal rendelkezo varosok a kovetkezok:
-- - arad
-- - kolozsvár
-- - nagyvarad
-- - temesvar
-- ● Amennyiben nincs olyan város, amelyiknek a népesség értéke egy adott n
-- értéknél nagyobb, a következő a kimenet: “Nincs x erteknel nagyobb nepesseg
-- ertekkel rendelkezo varos.”

valogat varosok n = sort [nev | (nev, ertek) <- varosok, ertek > n]

valogat2 varosok n = sort $ map fst $ filter (\(nev, ertek) -> ertek > n) varosok

fel1 = do
    let varosok = 
            [ ("sepsiszentgyorgy", 54000), 
              ("kolozsvár", 330000),
              ("marosvasarhely", 130000), 
              ("temesvar", 310000), 
              ("arad", 160000),
              ("gyergyoszentmiklos", 18000), 
              ("nagyvarad",196000)
            ]
        n = 15000
        nepesseg = map snd varosok
        varosNevek = valogat varosok n 
    if null varosNevek
        then putStrLn ("Nincs " ++ show n ++ " erteknel nagyobb nepesseg ertekkel rendelkezo varos.")
        else do
            mapM_ (\(v, n) -> putStr (v ++ " " ++ show n ++ ", ")) varosok
            putStrLn "A(z) 150000 nepesseg erteknel nagyobbal rendelkezo varosok a kovetkezok:"
            mapM_ (\v -> putStrLn ("- " ++ v)) varosNevek
            putStrLn $ intercalate ", " varosNevek
            putStrLn $ unwords varosNevek
            putStrLn $ intercalate ", " $ map show nepesseg



-- 2. Írjunk egy Haskell függvényt, amely meghatározza egy bemeneti egész
-- számokat tartalmazó lista azon elemeit, amelyek nem tartalmazzák a 0
-- számjegyet. Az eredmény számokat szóközzel elválasztva írjuk ki a
-- képernyőre.
-- Például:
-- ● Bemenet: [17603, 4005, 3223, 816252, 70, 23561, 9018007, 807, 61, 300]
-- ● Kimenet: A 0 szamjegyet nem tartalmazo szamok a kovetkezok: 3223 816252
-- 23561 61
-- ● Amennyiben nincsenek ilyen számok, a kimenet a következő: “Nincsenek
-- olyan szamok, amelyek nem tartalmazzak a 0 szamjegyet.”

nincsNulla n = notElem '0' (show n)

nincsNulla2 n
    | n < 10 && n == 0 = False
    | n < 10 && n /= 0 = True
    | mod n 10 == 0 = False
    | otherwise = nincsNulla2 (div n 10)

fel2 = do
    let ls = [17603, 4005, 3223, 816252, 70, 23561, 9018007, 807, 61, 300]
        ls1 = filter nincsNulla ls
        ls2 = filter (\n -> '0' `notElem` show n) ls
        ls3 = filter nincsNulla2 ls
    if null ls3
        then putStrLn "Nincsenek olyan szamok, amelyek nem tartalmazzak a 0 szamjegyet."
        else do
            putStr "A 0 szamjegyet nem tartalmazo szamok a kovetkezok:"
            mapM_ (\n -> putStr (show n ++ " ")) ls1



-- 3. Egy listában karakterláncok vannak, írjunk egy Haskell programot, amely kiírja
-- azokat a karakterláncokat a képernyőre egymás alá rendezve ábécé
-- sorrendbe, amelyekben nincsenek számjegyek.
-- Például:

-- ● Bemenet: ["2023tuple", "function", "float", "higher-order", "variable10",
-- "may13be", "0recursion", "monad", "class"]
-- ● Kimenet:
-- A karakterlancok, amelyek nem tartalmaznak szamokat:
-- class
-- float
-- function
-- Higher-order
-- monad
-- ● Amennyiben nincsenek ilyen karakterláncok, a kimenet a következő:
-- “Nincsenek olyan karakterlancok, amelyek nem tartalmaznak szamot.”

nincsSzam n = not (any isDigit n)

nincsSzam2 n = all isAlpha n

nincsSzam3 n = if filter isDigit n == "" then True else False

fel3 = do
    let ls = ["2023tuple", "function", "float", "higher-order", "variable10", "may13be", "0recursion", "monad", "class"]
        ls1 = filter nincsSzam ls
        ls2 = sort [szo | szo <- ls, not (any isDigit szo)]
        ls3 = sort $ filter (\szo -> not (any isDigit szo)) ls
    if null ls1
        then putStrLn "Nincsenek olyan karakterlancok, amelyek nem tartalmaznak szamot."
        else do
            putStrLn "A karakterlancok, amelyek nem tartalmaznak szamokat:"
            mapM_ putStrLn ls3  --(sort ls1)



-- 4. Írjunk egy Haskell programot, amely meghatározza, hogy az s karakterláncnak
-- melyek a szomszédjai az lsS karakterláncokat tartalmazó listából, ahol egy
-- karakterlánc szomszédjait az ábécé sorrend szerinti kell érteni.
-- Például:
-- ● Bemenet:
-- s = feri
-- lsS = Mari Zsuzsa szidi Lori kata feri teri Dani zsolti
-- ● Kimenet: feri baloldali szomszedja Zsuzsa, jobboldali szomszedja pedig kata

ketoldaliSzomszedok s lsS = aux (sort lsS)
    where 
        aux (x : y : z : ve)
            | x == s = [y]
            | y == s = [x, z]
            | z == s = [y]
            | otherwise = aux ve

ketoldaliSzomszedok2 s lsS
    | keresettIndex == 0 = [lsS !! (keresettIndex + 1)]
    | keresettIndex == length lsS - 1 = [lsS !! (keresettIndex - 1)]
    | otherwise = [lsS !! (keresettIndex - 1), lsS !! (keresettIndex + 1)]
    where
        keresettIndex = aux (zip [0 ..] lsS)
        aux ((idx, nev) : ve)
            | nev == s = idx
            | otherwise = aux ve

fel4 = do
    let s = "feri"
        lsS = ["Mari Zsuzsa szidi Lori kata feri teri Dani zsolti"]
        szomszedok = ketoldaliSzomszedok s lsS
        szomszedok2 = ketoldaliSzomszedok2 s (sort lsS) 
        sz1 = head szomszedok
        sz2 = last szomszedok
    if sz1 == sz2
        then putStrLn (s ++ " 1 szomszedja van, " ++ sz1)
        else putStrLn (s ++ " baloldali szomszedja " ++ sz1 ++ ", jobboldali szomszedja " ++ sz2)



-- 5. Egy [(String, Int, Int)] típusú lista eleme egy telefon márkanevet, egy eladási
-- értéket, és egy árat tartalmaz. Írjunk egy Haskell programot, amely
-- meghatározza azokat a telefonokat, amelyekből a legtöbbet adtak el, illetve
-- mennyi volt ez az érték. Az eredmény márkaneveket rendezve egymás alá
-- írjuk, amelyek elé írjuk ki egy kisérő szöveggel együtt a maximális eladási
-- értéket.
-- Például:
-- ● Bemenet: [("iphoneS1", 20, 2500), ("huaweiS1", 30, 1700), ("huaweiS2", 25,
-- 3100), ("samsungA1", 30, 2000), ("nokia", 10, 1900), ("iphoneS2", 10, 2200),
-- ("samsungA2", 15, 1650), ("iphone3", 30, 1800)]
-- ● Kimenet: A maximalis eladasi ertek 30. A telefonok, amelyeknek ennyi az
-- eladasi erteke a kovetkezok:
-- - iphone3
-- - huaweiS1
-- - samsungA1

thrd (_, _, x) = x

sndThrd (_, x, _) = x

telefonok ls = sort [nev | (nev, eladEr, ar) <- ls, eladEr == maxEladEr]
    where 
        maxEladEr = maximum (map (\(_, ee, _) -> ee) ls)
        maxEladEr2 = maximum (map sndThrd ls)

printTelefonok ls = mapM_ (\t -> putStrLn ("- " ++ t)) ls

fel5 = do
    let ls =
          [ ("iphoneS1", 20, 2500), 
            ("huaweiS1", 30, 1700), 
            ("huaweiS2", 25, 3100), 
            ("samsungA1", 30, 2000), 
            ("nokia", 10, 1900), 
            ("iphoneS2", 10, 2200),
            ("samsungA2", 15, 1650), 
            ("iphone3", 30, 1800)
         ]
        maxElement = maximum $ map (\(_, eladErtek, _) -> eladErtek) ls
        ls1 = filter (\(nev, eladErtek, _) -> eladErtek == maxElement) ls
        ls2 = telefonok ls
    putStrLn ("A maximalis eladasi ertek " ++ show maxElement ++ ". A telefonok, amelyeknek ennyi az eladasi erteke a kovetkezok:")
    mapM_ (\(nev, _, _) -> putStrLn nev) ls1
    printTelefonok ls2



-- 6. Írj egy Haskell függvényt, melynek egy lista a bemenete, és megadja azokat a
-- számokat, amelyek előfordulási száma páratlan. Az eredményt írasd ki a
-- példában szereplő formában, előfordulási érték szerint rendezve.
-- Például:
-- ● Bemenet: [7]
-- ● Kimenet: Elofordulas: 1 -> Ertek: 7
-- ● Bemenet: [1, 1, 2]
-- ● Kimenet: Elofordulas: 1 -> Ertek: 2
-- ● Bemenet: [1, 1]
-- ● Kimenet: Nincs paratlan elofordulasi ertekkel rendelkezo szam.
-- ● Bemenet: [1, 1, 2, 3, 4, 2, 6, 2, 4, 4, 2, 6, 7, 6, 6, 2]
-- ● Kimenet:
-- Elofordulas: 1 -> Ertek: 3
-- Elofordulas: 1 -> Ertek: 7
-- Elofordulas: 3 -> Ertek: 4
-- Elofordulas: 5 -> Ertek: 2

fel6 = do
    let ls1 = [7]
        ls2 = [1, 1, 2]
        ls3 = [1, 1]
        ls4 = [1, 1, 2, 3, 4, 2, 6, 2, 4, 4, 2, 6, 7, 6, 6, 2]
        megoldas = map (\bLs -> (head bLs, length bLs)) $ filter (\bLs -> odd (length bLs)) $ group $ sort ls4
    if null megoldas
        then putStrLn "Nincs paratlan elofordulasi ertekkel rendelkezo szam."
        else
            mapM_ (\(szam, elof) -> putStrLn ("Elofordulas: " ++ show elof ++ " -> Ertek: " ++ show szam)) megoldas