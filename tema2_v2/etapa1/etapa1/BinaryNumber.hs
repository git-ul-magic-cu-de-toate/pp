module BinaryNumber where

import Data.List
import Data.Tuple (swap)

{-
    Reprezentarea unui număr binar ca listă finită sau infinită de biți.
    Primul bit este cel mai puțin semnificativ.

    De exemplu, 6 poate fi reprezentat ca [0, 1, 1, 0, 0, ...].

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type BinaryNumber = [Int]

{-
    *** TODO ***

    Transformă un număr din reprezentarea binară finită în cea zecimală.
    
    Constrângeri: funcția trebuie definită
    * în stil point-free (fără explicitarea parametrului formal)
    * utilizând o funcțională (fără recursivitate explicită).

    Exemple:
    
    > toDecimal [0,1,1]
    6
-}
{-
    fold -> funcție de reducere asupra unei liste, începând de la ultimul element și ajungând la primul
    \bit acc -> acc * 2 + bit_curent -> ia ca argumente un bit din numărul binar și valoarea acumulată până în acel moment și calculează valoarea zecimală corespunzătoare acestora
    acc -> inițializat cu 0
-}

toDecimal :: BinaryNumber -> Int
toDecimal = foldr (\bit acc -> acc * 2 + bit) 0

{-
    *** TODO ***

    Transformă un număr din reprezentarea zecimală în cea binară infinită.

    Constrângeri: pt bonus 10p, funcția trebuie definită
    * în stil point-free (fără explicitarea parametrului formal)
    * utilizând funcționala unfoldr (fără recursivitate explicită).

    Spre deosebire de foldr, care împăturește o listă la o singură valoare,
    unfoldr desfășoară procesul invers, construind o listă pe baza unei valori
    inițiale.
    
    Hint: divMod.

    Exemple:
    
    > take 10 $ toBinary 6
    [0,1,1,0,0,0,0,0,0,0]
-}

{-
    toBinary -> sub forma unei liste de biți 
    caz 1 -> numărul zecimal este 0 -> funcția întoarce lista cu un singur element, 0
    caz 2 -> myBin -> primește un număr zecimal și întoarce reprezentarea binară sub forma unei liste de biți
                   -> adaugă biții în ordine inversă, începând cu cel mai puțin semnificativ bit
                * o listă vidă (reprezentarea binară a lui 0 este o listă vidă)
                * nr par -> adaugă un 0 la sfârșitul listei întoarse
                * nr impar -> adaugă un 1 la sfârșitul listei întoarse
-}

toBinary :: Int -> BinaryNumber
toBinary = unfoldr (\n -> Just (snd (divMod n 2), div n 2))
    
{-
    *** TODO ***

    Incrementează cu 1 reprezentarea finită sau infinită a unui număr binar.
    Atenție la transport!

    Constrângeri: utilizați
    * recursivitate explicită
    * pattern matching.

    Exemple:

    > inc [0,1,1] 
    [1,1,1]

    > inc [1,1,1]
    [0,0,0,1]
-}

{-
    recursivitatea explicită -> apelul recursiv se face prin apelarea directă a funcției inc
    pattern matching -> trateaza cazurile :
    Caz 1: lista este vida -> se adauga un 1 
    Caz 2: daca incepe cu 0 -> se adauga 1 la inceputul listei
    Caz 3: daca incepe cu 1 -> se adauga 0 la inceputul listei
-}

inc :: BinaryNumber -> BinaryNumber
inc [] = [1]
inc (x : aux)
  | x == 0    = 1 : aux
  | otherwise = 0 : inc aux

{-
   *** TODO ***

    Decrementează cu 1 reprezentarea finită sau infinită a unui număr binar.
    Atenție la împrumut!

    Constrângeri: utilizați
    * recursivitate explicită
    * pattern matching.

    Exemple:

    > dec [1,1,1]
    [0,1,1]

    > dec [0,0,0,1]
    [1,1,1]
-}

{-
    -> primul element = 0 && lista conține un singur element =>  lista întoarce [0]
    -> primul element = 0 && lista conține mai mult de un element => numărul binar este un număr pozitiv diferit de zero
                    * funcția aplică recursiv funcția dec asupra listei fără primul element + concatenează 1 la începutul listei obținute
    -> primul element = 1 => numărul binar este un număr negativ (complementul la 2 al numărului pozitiv cu aceeași valoare absolută)
                    * funcția schimbă 1 cu 0 la începutul listei primite ca argument

-}

dec :: BinaryNumber -> BinaryNumber
dec (0 : []) = [0] -- lista trebuie sa aiba cel putin o cifra 0 la inceput
dec (0 : aux) = let dec_aux = dec aux
             in (1 : dec_aux)
dec (1 : aux) = (0 : aux)

{-
    *** TODO ***

    Adună două numere binare, asumând reprezentări infinite, pentru ușurința
    aplicării funcționalelor.

    Constrângeri: utilizați
    * where sau let
    * pt bonus 10p, funcționala mapAccumL (fără recursivitate explicită).

    mapAccumL are tipul (a -> b -> (a, c)) -> a -> [b] -> (a, [c]).
    Așa cum sugerează numele, combină comportamentele funcționalelor:
    * map, transformând element cu element [b] în [c]
    * foldl, utilizând în același timp un acumulator de tipul a.

    Exemple:

    > take 10 $ add (toBinary 74) (toBinary 123)
    [1,0,1,0,0,0,1,1,0,0]
    
    > toDecimal $ take 10 $ add (toBinary 74) (toBinary 123)
    197
-}

{-
    add -> adună folosind algoritmul de adunare în baza 2
    zipLongest -> returnează o listă de tupluri reprezentând fiecare cifră binară a celor două numere
            * lungimea numerelor este diferită => folosim pad pentru a completa numerele cu zero-uri astfel încât acestea să aibă aceeași lungime
            * mapAccumL => aplică funcția pe perechea formată din primul element al listei și o valoare de stare 
                                + continuă cu perechea formată din al doilea element al listei și noua valoare de stare
                        => întoarce o pereche formată din ultima valoare de stare & o listă de cifre binare => rezultatul adunării numerelor
    variabila c ->  bit-ul de carry inițial & inițializat cu 0
    variabilele a & b -> cifrele binare din tuplu (s = suma a + b + c )
    snd ->  extrage doar lista de cifre binare din perechea returnată de mapAccumL
-}

add :: BinaryNumber -> BinaryNumber -> BinaryNumber
add bits1 bits2 = snd $ mapAccumL (\c (a, b) -> let s = a + b + c
                                       in (s `div` 2 , s `mod` 2)) 0 (zipLongest bits1 bits2)
    where
      zipLongest x_aux y_aux = zip (pad x_aux) (pad y_aux)
      pad x_aux = x_aux ++ repeat 0


{-
    *** TODO ***

    În pregătirea operației de înmulțire a două numere binare, cu reprezentări
    infinite, stivuiește copii deplasate la dreapta ale lui bits1, înmulțite
    cu bit-ul curent din bits2. Deplasarea se face adăugând la stânga lui bits1
    un număr de 0-uri dat de numărul liniei curente. Întoarce o listă infinită
    de liste infinite.

    Vizual:

    0 1 1 0 ... *   <- bits1
    1 0 1 0 ...     <- bits2
    -----------
   |0 1 1 0 ...        înmulțire bits1 cu 1 și deplasare 0 poziții la dreapta
    0|0 0 0 0 ...      înmulțire bits1 cu 0 și deplasare 1 poziție la dreapta
    0 0|0 1 1 0 ...    înmulțire bits1 cu 1 și deplasare 2 poziții la dreapta

    Constrângeri:
    * Corpul funcției trebuie să fie un list comprehension.
    * Nu utilizați recursivitate explicită.

    Hint: iterate pt generarea secvențelor de deplasare spre dreapta cu biți 0.

    Exemple:

    (exemplul vizual)
    > take 3 $ map (take 6) $ stack (toBinary 6) (toBinary 5)
    [[0,1,1,0,0,0],[0,0,0,0,0,0],[0,0,0,1,1,0]]
-}


{-
    stack -> întoarce o listă infinită de liste infinite
          -> fiecare element din lista interioară = bits1 * (un bit din bits2) 
                   * deplasată la dreapta cu un număr de poziții dat de indexul curent din bits2
          -> creează o listă de lungime dată de bits2 , unde fiecare element este o listă de lungimea lui bits1
    b ->  bit-ul curent din bits2 
    i ->  indexul curent din bits2
-}
zeros :: [Int]
zeros = 0 : zeros

stack :: BinaryNumber -> BinaryNumber -> [BinaryNumber]
stack bits1 bits2 = [[if b == 1 then x else 0 
  | x <- replicate i 0 ++ bits1] 
  | (i, b) <- zip [0..] bits2]


{-
    *** TODO ***

    Întoarce o listă infinită de numere binare, care pe poziția i >= 0 conține
    rezultatul înmulțirii lui bits1 cu numărul format din primii i biți
    ai lui bits2, i.e. suma primelor i linii întoarse de stack.

    Constrângeri:
    * Utilizați funcționala scanl (fără recursivitate explicită).

    Spre deosebire de foldl, care întoarce acumulatorul final, scanl întoarce
    o listă cu toate acumulatoarele intermediare.

    Exemple:
    
    > take 5 $ map (toDecimal . take 10) $ multiply (toBinary 6) (toBinary 5) 
    [0,6,6,30,30]
-}

{-
    multiply -> aplică mai întâi funcția stack pentru a crea o listă de numere binare prin deplasarea în dreapta a lui bits1 
                  + înmulțirea fiecărui bit din bits2 cu fiecare element corespunzător din lista de numere deplasate
    scanl1 -> aplica funcția add între elementele consecutive din lista obținută prin stack
    add -> adună două numere binare de aceeași lungime
    rezultat -> listă infinită de numere binare
                    * fiecare reprezentând suma acumulată până la etapa respectivă a înmulțiri
-}

stackedBits :: BinaryNumber -> BinaryNumber -> [BinaryNumber]
stackedBits bits1 bits2 = stack bits1 bits2

multiply :: BinaryNumber -> BinaryNumber -> [BinaryNumber]
multiply bits1 bits2 = scanl addBinaryNumbers zeros (stackedBits bits1 bits2)
  where
    addBinaryNumbers acc curr = zipWith (\a b -> (a + b) `mod` 2) acc curr
    zeros = repeat 0

{-
    *** TODO ***

    Întrebare de reflecție, la care veți răspunde la prezentarea temei.

    Având în vedere că liniile întoarse de stack care conțin exclusiv biți 0
    nu contribuie la rezultatul unei înmulțiri, să presupunem că modificăm
    definiția funcției stack astfel încât să construiască doar liniile utile;
    de exemplu, folosind filter sau pattern matching în list comprehension
    pt a păstra doar biții 1 din bits2. Ce probleme ar putea crea această
    abordare?
-}
