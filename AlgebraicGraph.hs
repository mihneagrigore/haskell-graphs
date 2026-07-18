module AlgebraicGraph where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable

data AlgebraicGraph a
    = Empty
    | Node { label :: a }
    | Overlay { left :: AlgebraicGraph a, right :: AlgebraicGraph a }
    | Connect { left :: AlgebraicGraph a, right :: AlgebraicGraph a }
    deriving (Eq)

-- (1, 2), (1, 3)
-- 1 * (2 + 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
-- 1 * (2 * 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
*** TODO 1 (15p) ***

Instanțiați clasa Foldable cu constructorul de tip AlgebraicGraph.
Nodurile trebuie prelucrate în ordinea în care apar în graf.

Odată ce implementați funcția foldr, multe alte funcții predefinite
care se bazează pe ea devin disponibile.

Exemple:

>>> toList triangle
[1,2,3]

>>> length triangle
3

>>> sum triangle
6

>>> maximum triangle
3
-}
instance Foldable AlgebraicGraph where
    foldr :: (a -> b -> b) -> b -> AlgebraicGraph a -> b
    foldr f acc graph = go graph acc
        where
            go Empty acc = acc
            go (Node x) acc = f x acc
            go (Overlay left right) acc = go left (go right acc)
            go (Connect left right) acc = go left (go right acc)


{-
*** TODO 2 (5p) ***

Reimplementați funcția nodes folosind foldr.

CONSTRÂNGERI:

* NU puteți obține în prealabil lista nodurilor (de exemplu, folosind toList).
* Utilizați stilul point-free.
-}
nodesWithFoldr :: Ord a => AlgebraicGraph a -> Set a
nodesWithFoldr = foldr Set.insert Set.empty

{-
*** TODO 3 (20p) ***

Este funcția edges din etapele 2 și 3 compozițională? Cu alte cuvinte:

* Asupra subgrafurilor se aplică numai funcția edges sau și alte funcții?
* Funcția edges însăși se aplică numai asupra subgrafurilor sau și asupra
  altor parametri?

Din păcate, veți oberva că nu este compozițională, întrucât asupra subgrafurilor
se aplică și funcția nodes, nu numai funcția edges. În plus, subgrafurile sunt
parcurse repetat, atât de edges, cât și de nodes, ceea ce duce la o eficiență
scăzută.

Utilizați tupling și equational reasoning pentru a o transforma într-o variantă
compozițională, numită nodesEdges. Mai precis, impuneți proprietatea

nodesEdges graph = (nodes graph, edges graph),

și derivați mai jos o definiție mai eficientă a funcției nodesEdges,
care să parcurgă graful o singură dată. Folosiți definițiile funcțiilor
nodes și edges din etapele 2 și 3.

DERIVARE:

Cazul Empty:

    nodesEdges Empty
    = {- proprietatea nodesEdges -}
    (nodes Empty, edges Empty)
    = {- definitiile nodes si edges -}
    (Set.empty, Set.empty)

Cazul Node:

    nodesEdges (Node x)
    = {- proprietatea nodesEdges -}
    (nodes (Node x), edges (Node x))
    = {- definitiile nodes si edges -}
    (Set.singleton x, Set.empty)

Cazul Overlay:

    nodesEdges (Overlay l r)
    = {- proprietatea nodesEdges -}
    (nodes (Overlay l r), edges (Overlay l r))
    = {- definitiile nodes si edges -}
    (Set.union (nodes l) (nodes r),
     Set.union (edges l) (edges r))
    = {- izolam aplicatiile recursive -}
    let (nl, el) = (nodes l, edges l)
        (nr, er) = (nodes r, edges r)
    in (Set.union nl nr, Set.union el er)
    = {- proprietatea nodesEdges citita invers -}
    let (nl, el) = nodesEdges l
        (nr, er) = nodesEdges r
    in (Set.union nl nr, Set.union el er)

Cazul Connect:

    nodesEdges (Connect l r)
    = {- proprietatea nodesEdges -}
    (nodes (Connect l r), edges (Connect l r))
    = {- definitiile nodes si edges -}
    (Set.union (nodes l) (nodes r),
     Set.union (Set.union (edges l) (edges r))
               (Set.cartesianProduct (nodes l) (nodes r)))
    = {- izolam aplicatiile recursive -}
    let (nl, el) = (nodes l, edges l)
        (nr, er) = (nodes r, edges r)
        cross = Set.cartesianProduct nl nr
    in (Set.union nl nr, Set.union (Set.union el er) cross)
    = {- proprietatea nodesEdges citita invers -}
    let (nl, el) = nodesEdges l
        (nr, er) = nodesEdges r
        cross = Set.cartesianProduct nl nr
    in (Set.union nl nr, Set.union (Set.union el er) cross)

-}
nodesEdges :: Ord a => AlgebraicGraph a -> (Set a, Set (a, a))
nodesEdges graph = case graph of

    Empty -> (Set.empty, Set.empty)

    Node x ->
        (Set.singleton x, Set.empty)

    Overlay l r ->
        let (nl, el) = nodesEdges l
            (nr, er) = nodesEdges r
        in (Set.union nl nr, Set.union el er)

    Connect l r ->
        let (nl, el) = nodesEdges l
            (nr, er) = nodesEdges r
            cross = Set.cartesianProduct nl nr
        in (Set.union nl nr, Set.union el er `Set.union` cross)

{-
Din păcate, deși nodesEdges este compozițională, ea nu poate fi implementată
cu foldr, întrucât cea din urmă expune o vedere liniară asupra grafului,
ascunzând structura sa ierarhică, cu subgrafuri combinate prin Overlay
și Connect.

În plus, deși nodesEdges este mai eficientă decât edges, pare că prețul plătit
pentru eficiența sporită este pierderea modularității și a reutilizării,
întrucât funcția nodes a trebuit reimplementată în definiția lui nodesEdges.

În vederea beneficierii atât de eficiență, cât și de modularitate/reutilizare,
soluția este definirea unui mecanism mai expresiv de reducere (folding) a
grafului, care să expună structura sa ierarhică.

Tipul de date AlgebraicGraphFolder reprezintă o colecție de funcții care
permit reducerea diverselor forme pe care le poate lua graful, presupunând
că subgrafurile au fost deja reduse recursiv.

Semnificația variabilelor de tip:

* a = tipul nodurilor din graf (la fel ca în AlgebraicGraph a)
* b = tipul rezultatului reducerii subgrafurilor
* c = tipul rezultatului reducerii grafului curent
-}
data AlgebraicGraphFolder a b c = AlgebraicGraphFolder
    { foldEmpty   :: c
    , foldNode    :: a -> c
    , foldOverlay :: b -> b -> c
    , foldConnect :: b -> b -> c
    }

{-
În măsura în care tipurile reducerilor subgrafurilor și a grafului curent
coincid (b), putem reduce întregul graf la același tip b. Observați parametrul
cu tipul (AlgebraicGraphFolder a b b).
-}
foldAlgebraicGraph :: AlgebraicGraphFolder a b b -> AlgebraicGraph a -> b
foldAlgebraicGraph folder = go
  where
    go Empty = foldEmpty folder
    go (Node node) = foldNode folder node
    go (Overlay g1 g2) = foldOverlay folder (go g1) (go g2)
    go (Connect g1 g2) = foldConnect folder (go g1) (go g2)

{-
*** TODO 4 (15p) ***

Reimplementați funcția nodes folosind noul mecanism de reducere. Propriu-zis,
trebuie să implementați doar AlgrabraicGraphFolder-ul.

CONSTRÂNGERI:

* Toate funcțiile din AlgebraicGraphFolder trebuie implementate folosind stilul
  point-free.

>>> nodes triangle
fromList [1,2,3]
-}
nodes :: Ord a => AlgebraicGraph a -> Set a
nodes = foldAlgebraicGraph nodesFolder

nodesFolder :: Ord a => AlgebraicGraphFolder a (Set a) (Set a)
nodesFolder = AlgebraicGraphFolder
    { foldEmpty   = Set.empty  -- c            =  Set a
    , foldNode    = Set.singleton  -- a -> c       =  a -> Set a
    , foldOverlay = Set.union  -- b -> b -> c  =  Set a -> Set a -> Set a
    , foldConnect = Set.union  -- b -> b -> c  =  Set a -> Set a -> Set a
    }

{-
*** TODO 5 (15p) ***

Implementați funcția isNode, care verifică dacă un nod aparține într-adevăr
unui graf, utilizând noul mecanism de reducere. Propriu-zis, trebuie să
implementați doar AlgrabraicGraphFolder-ul.

CONSTRÂNGERI:

* Toate funcțiile din AlgebraicGraphFolder trebuie implementate folosind stilul
  point-free.

Exemple:

>>> isNode 1 triangle
True

>>> isNode 4 triangle
False
-}
isNode :: Eq a => a -> AlgebraicGraph a -> Bool
isNode = foldAlgebraicGraph . isNodeFolder

isNodeFolder :: Eq a => a -> AlgebraicGraphFolder a Bool Bool
isNodeFolder node = AlgebraicGraphFolder
    { foldEmpty   = False  -- c            =  Bool
    , foldNode    = (== node)   -- a -> c       =  a -> Bool
    , foldOverlay = (||)  -- b -> b -> c  =  Bool -> Bool -> Bool
    , foldConnect = (||)  -- b -> b -> c  =  Bool -> Bool -> Bool
    }

{-
Operatorul (<+>) combină două foldere INDEPENDENTE, care reduc graful la tipuri
diferite (b și c), într-un folder care reduce graful la o pereche de tipuri
(b, c).

infixl (l = left) asigură asociativitatea la stânga a operatorului (vedeți
exemplul 2).

Exemple:

>>> foldAlgebraicGraph (nodesFolder <+> isNodeFolder 2) triangle
(fromList [1,2,3],True)

>>> foldAlgebraicGraph (nodesFolder <+> isNodeFolder 2 <+> nodesFolder) triangle
((fromList [1,2,3],True),fromList [1,2,3])
-}
infixl 5 <+>
(<+>) :: AlgebraicGraphFolder a b b
      -> AlgebraicGraphFolder a c c
      -> AlgebraicGraphFolder a (b, c) (b, c)
folder1 <+> folder2 = AlgebraicGraphFolder
    { foldEmpty = (foldEmpty folder1, foldEmpty folder2)
    , foldNode = \node -> (foldNode folder1 node, foldNode folder2 node)
    , foldOverlay = \(b1, c1) (b2, c2) ->
        (foldOverlay folder1 b1 b2, foldOverlay folder2 c1 c2)
    , foldConnect = \(b1, c1) (b2, c2) ->
        (foldConnect folder1 b1 b2, foldConnect folder2 c1 c2)
    }

{-
Operatorul (>.>) combină două foldere SEMIDEPENDENTE, care reduc graful la
tipuri diferite (b și c), într-un folder care reduce graful la o pereche de
tipuri (b, c).

Parantezele unghiulare indică sensul dependenței, de la stânga la dreapta.
Primul folder este independent, reducând graful la tipul b, în timp ce al
doilea folder este dependent de primul, și reduce graful la tipul c, pornind
de la informația calculată de AMBELE foldere, (b, c).

infixl (l = left) asigură asociativitatea la stânga a operatorului.
-}
infixl 5 >.>
(>.>) :: AlgebraicGraphFolder a  b     b
      -> AlgebraicGraphFolder a (b, c) c
      -> AlgebraicGraphFolder a (b, c) (b, c)
folder1 >.> folder2 = AlgebraicGraphFolder
    { foldEmpty = (foldEmpty folder1, foldEmpty folder2)
    , foldNode = \node -> (foldNode folder1 node, foldNode folder2 node)
    , foldOverlay = \(b1, c1) (b2, c2) ->
        (foldOverlay folder1 b1 b2, foldOverlay folder2 (b1, c1) (b2, c2))
    , foldConnect = \(b1, c1) (b2, c2) ->
        (foldConnect folder1 b1 b2, foldConnect folder2 (b1, c1) (b2, c2))
    }

{-
*** TODO 6 (20p) ***

Reimplementați funcția edges folosind noul mecanism de reducere. Propriu-zis,
trebuie să implementați doar AlgrabraicGraphFolder-ul.

De data aceasta, mulțimea de arce produsă de folder, Set (a, a), depinde
nu numai de mulțimile de muchii calculate pentru subgrafuri, Set (a, a), ci și
de mulțimile de noduri calculate pentru aceleași subgrafuri, Set a. Din acest
motiv, funcțiile din folder iau ca parametri perechi între o mulțime de noduri
și una de muchii, (Set a, Set (a, a)).

Observați că acum nu a mai trebuit să reimplementăm funcționalitatea lui nodes,
ca în funcția nodesEdges de mai sus. Am putut reutiliza funcționalitatea lui
nodes, dar nu la nivelul întregii funcții, ci doar la nivelul folderului său.

Exemple:

>>> edges triangle
fromList [(1,2),(1,3),(2,3)]
-}
edges :: Ord a => AlgebraicGraph a -> Set (a, a)
edges = snd . foldAlgebraicGraph (nodesFolder >.> edgesFolder)

edgesFolder :: Ord a => AlgebraicGraphFolder a (Set a, Set (a, a)) (Set (a, a))
edgesFolder = AlgebraicGraphFolder
      -- Set (a, a)
    { foldEmpty = Set.empty

      -- a -> Set (a, a)
    , foldNode = const Set.empty

      -- (Set a, Set (a, a)) -> (Set a, Set (a, a)) -> Set (a, a)
    , foldOverlay = \(_, edges1) (_, edges2) -> Set.union edges1 edges2

      -- (Set a, Set (a, a)) -> (Set a, Set (a, a)) -> Set (a, a)
    , foldConnect = \(nodes1, edges1) (nodes2, edges2) -> Set.unions [edges1, edges2, (Set.cartesianProduct nodes1 nodes2)]
    }
