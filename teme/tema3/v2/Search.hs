{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
--import Pipes
--import RollTheBall
import Data.Maybe
{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = Node { stare :: s 
                     , actiune :: Maybe a
                     , parinte :: Maybe (Node s a)
                     , adancime :: Int
                     , copii :: [Node s a] } deriving(Eq)

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}

nodeState :: Node s a -> s
nodeState nod = (stare nod)

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent nod = (parinte nod)

nodeDepth :: Node s a -> Int
nodeDepth nod = (adancime nod)

nodeAction :: Node s a -> Maybe a
nodeAction nod = (actiune nod)

nodeChildren :: Node s a -> [Node s a]
nodeChildren nod = (copii nod)

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}

createState :: (ProblemState s a) => s -> a -> Maybe (Node s a) -> Int -> Node s a
createState state action parent depth = current_node
    where
        current_node = Node state (Just action) parent depth children
        children = map (\(laction, lstate) -> createState lstate laction (Just current_node) (depth + 1)) (successors state)

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace initial = start_node
    where
        start_node = Node initial Nothing Nothing 0 children
        childrenlist = successors initial
        children = map (\(laction, lstate) -> createState lstate laction (Just start_node) 1) childrenlist
{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}

bfsHelper :: (Eq s) => ([Node s a], [Node s a]) -> [s] -> [([Node s a], [Node s a])]
bfsHelper prev@(_ , allnodes) visited
    | (null allnodes) = []
    | otherwise = [prev] ++ next
    where
        newvisited = visited ++ [nodeState current_node]
        current_node = head (allnodes)
        neighbours = nodeChildren current_node
        newadded = [ x | x <- neighbours, not ((nodeState x) `elem` newvisited)]
        newallnodes = (tail allnodes) ++ newadded
        next = bfsHelper (newadded, newallnodes) newvisited

bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs start = bfsHelper ([start], [start]) [] 

{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

hasIntersection :: (ProblemState s a, Eq s) => [Node s a] -> [Node s a] -> Bool
hasIntersection fr1 fr2 = length (filter (\x -> (nodeState x) `elem` (map nodeState fr1)) fr2) > 0

getIntersection :: (ProblemState s a, Eq s) => [Node s a] -> [Node s a] -> (Node s a, Node s a)
getIntersection list1 list2 = (state1, state2)
    where
        state1 = head (filter (\x -> (nodeState x) `elem` (map nodeState list2)) list1)
        state2 = head (filter (\x -> (nodeState x) == (nodeState state1)) list2)

bidirBFSHelper :: (ProblemState s a, Eq a, Eq s, Ord s) => [([Node s a], [Node s a])] -> [([Node s a], [Node s a])] -> (Node s a, Node s a)
bidirBFSHelper fr1 fr2
    | hasIntersection (fst (head fr1)) (snd (head fr2)) = getIntersection (fst (head fr1)) (snd (head fr2))
    | hasIntersection (fst (head fr2)) (snd (head fr1)) = getIntersection (fst (head fr2)) (snd (head fr1))
    | otherwise = bidirBFSHelper (tail fr1) (tail fr2)

bidirBFS ::(ProblemState s a, Eq a, Eq s, Ord s) => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS start finish = bidirBFSHelper (bfs start) (bfs finish)

{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

extractPath :: Node s a -> [(Maybe a, s)]
extractPath node
    | isNothing (nodeParent node) = [(nodeAction node, nodeState node)]
    | otherwise = extractPath (fromJust (nodeParent node)) ++ [(nodeAction node, nodeState node)]

{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

solve :: (ProblemState s a, Eq a, Eq s, Ord a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve initial final = firstpath ++ secondpath_updated
    where
        (node1, node2) = bidirBFS (createStateSpace initial) (createStateSpace final)
        firstpath = extractPath node1
        secondpath = reverse (extractPath node2)
        actions = map fromJust (map fst (init secondpath))
        states = map snd (tail secondpath)
        secondpath_updated = map (\x -> (Just (fst x), snd x)) (map reverseAction (zip actions states))
