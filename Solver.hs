module Solver {-(bruteForce, reduce, Solving(..))-} where

import Data.List
import Data.Maybe
import Utils
import Puzzle
import Graph
import Doors

bruteForce :: SolvingData -> [[Node]]
bruteForce d = map (expandPaths (paths d)) $ cleanUpSolutions $ bruteForceImpl d where
  cleanUpSolutions :: Ord a => [[a]] -> [[a]]
  cleanUpSolutions paths = nub [if first path < last path then path else reverse path | path <- paths]

bruteForceImpl :: SolvingData -> [[Node]]
bruteForceImpl d = concat [dfs d2 [d1] d1 ((delete d1 . allNodes . graph) d) compiledPaths
                       | (d1,d2) <- doorPairs (doorStatus d)] where
  compiledPaths :: [(Node,Node)]
  compiledPaths = map bothEnds (paths d)
  findPathAndOtherEnd :: [(Node,Node)] -> Node -> Maybe ((Node,Node), Node)
  findPathAndOtherEnd [] node = Nothing -- it's a little faster to have paths compiled into their (Node,Node) ends
  findPathAndOtherEnd (p@(f,l):ps) node = case () of
    _ | node == f -> Just (p,l)
    _ | node == l -> Just (p,f)
    _ -> findPathAndOtherEnd ps node
  dfs :: Node -> [Node] -> Node -> [Node] -> [(Node,Node)] -> [[Node]]
  dfs exitDoor rpath node [] ps = if exitDoor == node then [reverse rpath] else []
  dfs exitDoor rpath node remaining ps = if exitDoor == node then [] else
    case findPathAndOtherEnd ps node of
      Nothing -> concat [dfs exitDoor (n:rpath) n (delete n remaining) ps
                        | n <- neighbors (graph d) node, remaining `contains` n]
      Just (p,n) -> dfs exitDoor (n:rpath) n (delete n remaining) (delete p ps)


-- combinaison des différentes méthodes de recherche de solutions

data Solving a = NoSolution
               | NotBetter a
               | Better a

toMaybe NoSolution = Nothing
toMaybe (NotBetter d) = Just d
toMaybe (Better d) = Just d

extract = fromJust . toMaybe

instance Monad Solving where
  NoSolution >>= solveFunc = NoSolution
  NotBetter d >>= solveFunc = solveFunc d
  Better d >>= solveFunc = solveFunc d
  return = NotBetter

instance Show a => Show (Solving a) where
  show NoSolution = "No solution exists."
  show (NotBetter d) = show d
  show (Better d) = show d

type SearchResult = Solving SolvingData

converge :: (SolvingData -> SearchResult) -> SolvingData -> SearchResult
converge search d = case search d of
  Better d -> converge search d
  d -> d

type Strategy = String -- small DSL
reduce :: Strategy -> SolvingData -> SearchResult
reduce [] d = NotBetter d
reduce (c:'*':cs) d = converge (reduceFunction c) d >>= reduce cs -- TODO parenthèses pour converger une composition  
reduce (c:cs) d = reduceFunction c d >>= reduce cs

reduceFunction :: Char -> SolvingData -> SearchResult
reduceFunction '3' = reduceGraph3
reduceFunction 'e' = removeEdges
reduceFunction 'd' = deduceDoorStatus
reduceFunction 'l' = edgeOrLackOfNeighbors
reduceFunction 'b' = bouncing
reduceFunction '2' = reduceGraph2 -- l over 3*, comme "l" mais en réduisant via 3* avant de chercher l'impossibilité
--reduceFunction 'B' = reduceGraph2B -- b over 3*


{-

Pour l'instant j'ai un bruteforce, mais avant de l'exécuter je
simplifie le problème en déduisant des faits de la même manière que je
résous manuellement. Les techniques de résolution ont une lettre
associée.

3 : localisation des paths de longueur 3 (réduction des noeuds sans
porte avec seulement deux voisins). Immédiat.

b : localisation des paths de longueur 2 (aka "bouncing" : quand le
problème devient prouvablement impossible en enlevant un
edge). Paramétrable par le type de recherche d'impossibilité. Rendrait
obsolète la précédente via join, si elle n'était pas bien plus lente.

automatique en début :
exploitation de la parité : sur quels (x+y)`mod`2 sont les doors ?
(une fois, au début, dans puzzle, pas dans graph)


TODO: avant bruteforce, re-réduire après itération sur les paires de portes. (essayer sur excellentHandMadePuz ?)

TODO: si "le graphe sans un endpoint" est impossible, alors cet endpoint devient obligatoire

TODO: le contraire du bouncing : supposer qu'un edge quelconque est un path, et si c'est impossible, alors l'enlever du graphe.

-- graphe "prouvablement impossible" si :
-- doorStatus impossible (<2 possible doors, ou >2 obligatory doors)
-- >1 composantes fortement connexes

TODO: tenter de découper le graphe en deux "zones" de superficie pas trop inégale qui ne communiquent que via 1 ou 2 bottlenecks, résoudre/simplifier les sous-graphes, puis combiner ça

TODO: si deux sous-graphes ne sont connectés que par un seul edge,
alors il y a une door oblig de chaque côté

TODO: avant je pouvais écrire """reduce $ addWormhole2 (29,33) $ addWormhole2 (13,17) $ reduce (graphOf (multiple4x4RoomsWithEndpoints 4))""", voir comment faire maintenant

TODO: un éditeur de niveaux pour écrire des tests plus facilement. dans mes rêves.

TODO: tester Control.Concurrent.compete

trouver s'il existe un moyen plus efficace que le moyen naïf de savoir
quels edges d'un graphe créeraient des composantes connexes s'ils
étaient enlevés ?
  hmm c'est quoi le moyen naïf déjà ? O(n) ?
  trouver d'abord les cycles, puis essayer les edges pas entre deux noeuds d'un cycle ?

TO NOT DO: profiling, changer les structures de données en Array ou
autre, voir si c'est plus performant... ne pas faire ça parce que 1)
rester high-level c'est plus rigolo, 2) ça marche pas, je ne sais pas
faire, à chaque fois que j'essaye d'optimiser du haskell c'est juste
plus lent après (et mon code est moins lisible).

-}

-------------------------------------------------- déduit les portes obligatoires si elles n'ont qu'un voisin. rapide.

deduceDoorStatus :: SolvingData -> SearchResult
deduceDoorStatus d = let
  nodesThatMustBeDoors = [n | (n,ns) <- graph d, length ns == 1]
  in case setObligatoryDoors nodesThatMustBeDoors (doorStatus d) of
    Nothing -> NoSolution -- peut également trouver une impossibilité si plus de 3 doors obligatoires
    Just newDoors -> Better d { _doorStatus = newDoors }

-------------------------------------------------- trouve les edges en trop des portes dans des paths. rapide.

removeEdges :: SolvingData -> SearchResult
removeEdges d = let
  obligDoors = (obligatoryDoors . doorStatus) d
  extraEdgesOfObligatoryDoorsInPath = [(n, ns \\ [n'])
                                      | (n,ns) <- (graph d),
                                        obligDoors `contains` n,
                                        n' <- maybeToList (findOtherEnd (paths d) n)]
  findOtherEnd :: [Path] -> Node -> Maybe Node
  findOtherEnd [] n = Nothing
  findOtherEnd (p:ps) n = let (f,l) = bothEnds p in case () of
    _ | f == n -> Just l
    _ | l == n -> Just f
    _ -> findOtherEnd ps n
  reductions = [flip withoutEdge (n,n') | (n,ns) <- extraEdgesOfObligatoryDoorsInPath, n' <- ns]
  in if empty reductions then NotBetter d else Better $ foldr (.) id reductions d

-------------------------------------------------- trouve les paths de longeur 3. rapide.

reduceGraph3 :: SolvingData -> SearchResult
reduceGraph3 d =
  let reductions = [(n,ns) | (n,ns) <- graph d, not (possibleDoors (doorStatus d) `contains` n) && length ns == 2]
      reduce3NodesIntoPath (nw,[n1,n2]) = addPath [n1, nw, n2] d
  in case map reduce3NodesIntoPath reductions of
    [] -> NotBetter d
    (Nothing:_) -> NoSolution
    ((Just d):_) -> Better d

-------------------------------------------------- trouve des paths de longeur 2. lent.

turnEdgeIntoPathIfNoSolutionOtherwise :: (SolvingData -> Bool) -> SolvingData -> SearchResult
turnEdgeIntoPathIfNoSolutionOtherwise isImpossible d = firstReduction reducedGraphs d where
  reducedGraphs d = [addEdgePath edge d |
                     edge <- everyNonPathEdge d,
                     isImpossible (withoutEdge d edge)]
  everyNonPathEdge d =
    [(e1,e2) |
     (e1,es) <- graph d,
     e2 <- es,
     e1 < e2, -- only process one of the two symmetric edges
     not (any (\p -> p `contains` e1 && p `contains` e2) (paths d))]
  addEdgePath (n1,n2) = addPath [n1,n2]
  firstReduction reductions d = case reductions d of
    [] -> NotBetter d
    (Nothing:_) -> NoSolution
    ((Just d):_) -> Better d

-- Une version plus générale, et bien plus lente, de reduceGraph3.
-- Si on vient de passer "3*", ça ne trouvera guère que les path qui s'arrêtent à 1 d'une door sans autre edge.
edgeOrLackOfNeighbors :: SolvingData -> SearchResult
edgeOrLackOfNeighbors = turnEdgeIntoPathIfNoSolutionOtherwise obviouslyNoSolution1

bouncing :: SolvingData -> SearchResult
bouncing = turnEdgeIntoPathIfNoSolutionOtherwise obviouslyNoSolution2

reduceGraph2 :: SolvingData -> SearchResult
reduceGraph2 = turnEdgeIntoPathIfNoSolutionOtherwise (obviouslyNoSolution1 `onReduced` reduce "3*")

--reduceGraph2B :: SolvingData -> SearchResult
--reduceGraph2B = turnEdgeIntoPathIfNoSolutionOtherwise (obviouslyNoSolution2 `onReduced` reduce "3*")

onReduced :: (SolvingData -> Bool) -> (SolvingData -> SearchResult) -> (SolvingData -> Bool)
f `onReduced` g = \d -> case g d of
  NoSolution -> True
  NotBetter d -> f d
  Better d -> f d

-- idée d'amélio à creuser : gérer les invalidations de parties de
-- graphe, pour ne pas essayer toujours les mêmes réductions qui ne
-- marchent toujours pas, et accélérer "2*" (parcours des reducedGraphs en O(n))


obviouslyNoSolution :: SolvingData -> SearchResult -- SLOW
obviouslyNoSolution d =
  if obviouslyNoSolution1 d || obviouslyNoSolution2 d
  then NoSolution
  else NotBetter d

obviouslyNoSolution1 d = any (doesNotHaveEnoughNeighbors (doorStatus d)) (graph d)
obviouslyNoSolution2 d = any (hasOnlyTwoNeighborsThatAreTheEndsOfOnePath (paths d)) (graph d)

doesNotHaveEnoughNeighbors :: DoorStatus -> (Node, [Node]) -> Bool
doesNotHaveEnoughNeighbors ds (n,ns) = case () of
  _ | obligatoryDoors ds `contains` n -> length ns < 1 -- si un noeud est forcément une porte a moins de 1 voisin
  _ | possibleDoors ds `contains` n -> length ns < 1 -- si un noeud qui est peut-être une porte a moins de 1 voisin
  _ -> length ns < 2 -- si un noeud qui n'est pas une porte a - de 2 voisins (usuellement déjà réduit par reduceGraph3)

hasOnlyTwoNeighborsThatAreTheEndsOfOnePath ws (n,ns) = any (setEqual ns) $ map bothEnds ws where
  setEqual (n1:n2:[]) (f,l) = (n1 == f && n2 == l) || (n2 == f && n1 == l)
  setEqual _ (f,l) = False -- pas de solution si un noeud a seulement 2 voisins qui sont les extrémités d'un même path
