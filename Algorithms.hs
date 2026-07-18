module Algorithms where

import StandardGraph
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

type Graph a = StandardGraph a

{-
*** TODO 7 (30p) ***

Utilizând equational reasoning, rafinați implementarea dfs din etapa 1
pentru a obține implementarea eficientă bazată pe o stivă.

Pentru aceasta, generalizați funcția internă folosită în etapa 1 (să o numim
search), astfel încât să ia ca parametru nu doar nodul curent din care începe
parcurgerea, ci o listă de noduri (să numim noua funcție searchList). Mai
precis, impuneți proprietatea:

searchList :: Ord a => [a] -> [a]
searchList nodes = concatMap search nodes,

de unde rezultă că

search node = searchList [node].

De aici, derivați o nouă definiție mai eficientă pentru searchList, abordând
cazul de bază și cazul general.

DERIVARE:

Cazul de baza:

    searchList []
    = {- proprietatea searchList -}
    concatMap search []
    = {- definitia lui concatMap -}
    []

Cazul general:

    searchList (x : xs)
    = {- proprietatea searchList -}
    concatMap search (x : xs)
    = {- definitia lui concatMap -}
    search x ++ concatMap search xs
    = {- definitia lui search -}
    (x : concatMap search (Set.toList (outNeighbors x graph)))
        ++ concatMap search xs
    = {- proprietatea listelor: (x : ys) ++ zs = x : (ys ++ zs) -}
    x : (concatMap search (Set.toList (outNeighbors x graph))
        ++ concatMap search xs)
    = {- proprietatea concatMap: concatMap f a ++ concatMap f b
                              = concatMap f (a ++ b) -}
    x : concatMap search (Set.toList (outNeighbors x graph) ++ xs)
    = {- proprietatea searchList citita invers -}
    x : searchList (Set.toList (outNeighbors x graph) ++ xs)

Exemple:

>>> dfsStack 1 tree
[1,2,7,15,8,3,9,10,4,11,12,5,13,14,6]

>>> dfsStack 4 tree
[4,11,12]
-}
dfsStack :: Ord a => a -> Graph a -> [a]
dfsStack node graph = searchList [node]
    where
        searchList [] = []
        searchList (x : xs) = x : searchList (Set.toList (outNeighbors x graph) ++ xs)

{-
DEBUG

Funcții pentru debugging, care afișează un mesaj DEBUG de fiecare dată când
un element este implicat într-o operație de concatenare.

La fel ca (++), appendDebug este asociativă la dreapta în expresii de forma:

xs `appendDebug` ys `appendDebug` zs.

De exemplu, expresiile echivalente asociate la dreapta

[1,2] `appendDebug` ([3] `appendDebug` [4,5,6])
[1,2] `appendDebug`  [3] `appendDebug` [4,5,6]   (implicit la dreapta)

afișează utilizări unice ale elementelor:

[
DEBUG: 1
1,
DEBUG: 2
2,
DEBUG: 3
3,4,5,6],

în timp ce asocierea la stânga

([1,2] `appendDebug` [3]) `appendDebug` [4,5,6]

necesită utilizări repetate ale elementelor:

[
DEBUG: 1

DEBUG: 1
1,
DEBUG: 2

DEBUG: 2
2,
DEBUG: 3
3,4,5,6]
-}
infixr 5 `appendDebug`
appendDebug :: Show a => [a] -> [a] -> [a]
appendDebug xs ys = foldr (\x -> (trace ("\nDEBUG: " ++ show x) x :)) ys xs

concatMapDebug :: Show b => (a -> [b]) -> [a] -> [b]
concatMapDebug f = foldr (appendDebug . f) []
