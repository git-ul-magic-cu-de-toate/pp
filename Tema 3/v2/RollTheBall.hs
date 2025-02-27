{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs, MultiWayIf #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as A

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = Cell { tip :: Char } 
    deriving (Eq, Ord)

instance Show Cell where
    show (Cell t) = [t]
{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level { cornerRD :: Position
                    , cellsArray :: (A.Array Position Cell) } 
    deriving (Eq, Ord)

{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}

instance Show Level where 
    show (Level _ cells) = foldl displayCell "" listc ++ "\n"
        where
            listc = A.assocs cells
            displayCell acc cell
                | snd (fst cell) == 0 = acc ++ [endl] ++ show (snd cell)
                | otherwise = acc ++ show (snd cell)

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel (posX, posY) = Level (posX, posY) emptyCells
    where
        emptyC = Cell emptySpace
        emptyCells = A.array ((0, 0), (posX, posY))
                             [((i, j), emptyC) | i <- [0..posX], j <- [0..posY]]

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

addCell :: (Char, Position) -> Level -> Level
addCell (cellType, pos@(posX, posY)) lvl@(Level posRD@(posRDX, posRDY) cells)
    | posX <= posRDX && posX >= 0 && posY <= posRDY && posY >= 0 
        = Level posRD updatedCells
    | otherwise = lvl
    where
        cell = Cell cellType
        emptyC = Cell emptySpace
        updatedCells
            | cells A.! pos == emptyC = cells A.// [(pos, cell)]
            | otherwise = cells

{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel posRD [] = emptyLevel posRD
createLevel posRD listcells = foldr addCell emptyL listcells 
    where
        emptyL = emptyLevel posRD

{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}
moveCellhelper :: Position -> Position -> Level -> Level
moveCellhelper initpos nextpos@(nextx, nexty) lvl@(Level posRD@(rdx, rdy) cells)
    | nextx <= rdx && nextx >= 0 && nexty <= rdy && nexty >= 0 
        && ((((tip nextcell) `elem` winningCells) == False) && (((tip nextcell) `elem` startCells) == False))
        = Level posRD updatedCells
    | otherwise = lvl
    where
        cell = cells A.! initpos
        nextcell =  cells A.! nextpos
        emptyC = Cell emptySpace
        updatedCells
            | nextcell == emptyC || cell == emptyC = cells A.// [(nextpos, cell), (initpos, nextcell)]
            | otherwise = cells
    

moveCell :: Position -> Directions -> Level -> Level
moveCell pos@(x, y) dir lvl@(Level _ cells) 
    | ((tip cell) `elem` startCells) || ((tip cell) `elem` winningCells) = lvl
    | otherwise = move
    where
        cell = cells A.! pos
        move
            | dir == North = moveCellhelper pos (x - 1, y) lvl
            | dir == East = moveCellhelper pos (x, y + 1) lvl
            | dir == South = moveCellhelper pos (x + 1, y) lvl
            | dir == West = moveCellhelper pos (x, y - 1) lvl
            | otherwise = lvl
{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}
connection :: Cell -> Cell -> Directions -> Bool
connection (Cell tip1) (Cell tip2) dir
    | dir == South = checkSouth
    | dir == East = checkEast
    | dir == North = checkNorth
    | dir == West = checkWest
    | otherwise = False
    where
        connection_up = [verPipe, botLeft, botRight]
        connection_down = [verPipe, topLeft, topRight]
        connection_left = [horPipe, botRight, topRight]
        connection_right = [horPipe, botLeft, topLeft]
        checkSouth
            | ((tip1 `elem` connection_down) || tip1 == startDown) 
                && ((tip2 `elem` connection_up) || tip2 == winUp) = True
            | otherwise = False
        checkEast
            | ((tip1 `elem` connection_right) || tip1 == startRight)
                && ((tip2 `elem` connection_left) || tip2 == winLeft) = True
            | otherwise = False
        checkNorth
            | ((tip1 `elem` connection_up) || tip1 == startUp)
                && ((tip2 `elem` connection_down) || tip2 == winDown) = True
            | otherwise = False
        checkWest
            | ((tip1 `elem` connection_left) || tip1 == startLeft)
                && ((tip2 `elem` connection_right) || tip2 == winRight) = True
            | otherwise = False

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}

wonLevelHelper :: (Position, Cell) -> Directions -> Level -> Bool
wonLevelHelper ((posx, posy), cell@(Cell tip1)) dir lvl@(Level (posRDX, posRDY) cells)
    | tip1 `elem` winningCells = True
    | otherwise = if
        | dir == North -> if
            | (posx - 1 >= 0) && (connection cell nextcell North) -> wonLevelHelper ((posx - 1, posy), nextcell) nextdir lvl
            | otherwise -> False
        | dir == South -> if
            | (posx + 1 <= posRDX) && (connection cell nextcell South) -> wonLevelHelper ((posx + 1, posy), nextcell) nextdir lvl
            | otherwise -> False
        | dir == West -> if
            | (posy - 1 >= 0) && (connection cell nextcell West) -> wonLevelHelper ((posx, posy - 1), nextcell) nextdir lvl
            | otherwise -> False
        | otherwise -> if
            | (posy + 1 <= posRDY) && (connection cell nextcell East) -> wonLevelHelper ((posx, posy + 1), nextcell) nextdir lvl
            | otherwise -> False
    where 
        nextcell 
            | dir == North = (cells A.! (posx - 1, posy))
            | dir == South = (cells A.! (posx + 1, posy))
            | dir == East = (cells A.! (posx, posy + 1))
            | otherwise = (cells A.! (posx, posy - 1))
        nextdir
            | dir == North = if
                | (tip nextcell) == verPipe -> North
                | (tip nextcell) == topLeft -> East
                | otherwise -> West
            | dir == South = if
                | (tip nextcell) == verPipe -> South
                | (tip nextcell) == botLeft -> East
                | otherwise -> West
            | dir == East = if
                | (tip nextcell) == horPipe -> East
                | (tip nextcell) == botRight -> North
                | otherwise -> South
            | otherwise = if
                | (tip nextcell) == horPipe -> West
                | (tip nextcell) == topLeft -> South
                | otherwise -> North

wonLevel :: Level -> Bool
wonLevel lvl@(Level _ cells) = wonLevelHelper start dir lvl
    where
        cellList = A.assocs cells
        start = head [ x | x <- cellList, (tip (snd x)) `elem` startCells]
        dir
            | (tip (snd start)) == startDown = South
            | (tip (snd start)) == startUp = North
            | (tip (snd start)) == startLeft = West
            | otherwise = East

movedCell :: Position -> Level -> Directions -> Bool
movedCell pos lvl dir 
    | lvl == (moveCell pos dir lvl) = False
    | otherwise = True

oppositeDir :: Directions -> Directions
oppositeDir dir
    | dir == North = South
    | dir == South = North
    | dir == East = West
    | otherwise = East

getSuccessors :: Level -> (Position, Cell) -> [((Position, Directions), Level)]
getSuccessors lvl cellS = filter (\s -> movedCell poscellS lvl (oppositeDir (snd (fst s)))) directions
    where
        poscellS = fst cellS
        posX = fst (fst cellS)
        posY = snd (fst cellS)
        moveNorth = (((posX - 1, posY), South), moveCell poscellS North lvl)
        moveSouth = (((posX + 1, posY), North), moveCell poscellS South lvl)
        moveEast = (((posX, posY + 1), West), moveCell poscellS East lvl)
        moveWest = (((posX, posY - 1), East), moveCell poscellS West lvl)
        directions = [moveNorth, moveSouth, moveEast, moveWest]

instance ProblemState Level (Position, Directions) where
    successors lvl@(Level _ cells) = foldl (++) [] (map (getSuccessors lvl) emptycells)
        where
            listc = A.assocs cells
            emptyS = Cell emptySpace
            emptycells = [x | x <- listc, (snd x) == emptyS]
    
    isGoal lvl = wonLevel lvl

    reverseAction ((pos@(posX, posY), dir), lvl) = ((newpos, oppositeDir dir), (moveCell pos (oppositeDir dir) lvl))
        where
            newpos
                | dir == North = (posX - 1, posY)
                | dir == South = (posX + 1, posY)
                | dir == East = (posX, posY + 1)
                | otherwise = (posX, posY - 1)