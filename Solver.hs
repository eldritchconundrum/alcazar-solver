module Solver {-(bruteForce, reduce, Solving(..))-} where

import Data.List
import Data.Maybe
import Utils
import Puzzle
import Graph
import Doors
import Strategy

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
               | NotBetter a -- serait-ce plus simple de définir NotBetter sans argument ?
               | Better a

toMaybe NoSolution = Nothing
toMaybe (NotBetter d) = Just d
toMaybe (Better d) = Just d

extract = fromJust . toMaybe

{-
instance Monad Solving where -- I can haz do notation
  NoSolution >>= solveFunc = NoSolution
  NotBetter d >>= solveFunc = solveFunc d
  Better d >>= solveFunc = solveFunc d
  return = NotBetter
-}

instance Show a => Show (Solving a) where
  show NoSolution = "No solution exists."
  show (NotBetter d) = show d
  show (Better d) = show d

type SearchResult = Solving SolvingData

converge :: (SolvingData -> SearchResult) -> SolvingData -> SearchResult
converge search d = case search d of
  Better d -> converge search d
  d -> d

reduceList :: [SolvingData -> SearchResult] -> SolvingData -> SearchResult
reduceList fs d = reduceList' False fs d where -- preserve whether any of the reductions yielded an improvement
  reduceList' False [] d = NotBetter d
  reduceList' True [] d = Better d
  reduceList' isBetter (f:fs) d = case f d of
    NoSolution -> NoSolution
    NotBetter d -> reduceList' isBetter fs d
    Better d -> reduceList' True fs d


reduceStrategy :: String -> SolvingData -> SearchResult
reduceStrategy strat d = executeStrategy (parseStrategy strat) d where

  -- small DSL, see Strategy.hs
  executeStrategy :: Strategy -> SolvingData -> SearchResult
  executeStrategy (SingleReduction c) d = reduceFunction c d
  executeStrategy (Reductions strats) d = reduceList (map executeStrategy strats) d
  executeStrategy (Repetition strat) d = converge (executeStrategy strat) d

  reduceFunction :: Char -> SolvingData -> SearchResult
  reduceFunction '3' = reduceGraph3
  reduceFunction 'e' = removeEdges
  reduceFunction 'd' = deduceDoorStatus
  reduceFunction 'b' = bouncing
  reduceFunction 'l' = edgeOrLackOfNeighbors
  reduceFunction '2' = reduceGraph2 -- l over 3*, comme "l" mais en réduisant via 3* avant de chercher l'impossibilité
  --reduceFunction 'B' = reduceGraph2B -- b over 3*
  reduceFunction c = error $ "Non-exhaustive patterns in function reduceFunction: " ++ [c]


-- Attention ! Renvoyer Better quand on n'est pas sûr entraine une boucle infinie dans reduceList !

-------------------------------------------------- déduit les portes obligatoires si elles n'ont qu'un voisin. rapide.

deduceDoorStatus :: SolvingData -> SearchResult
deduceDoorStatus d = let
  nodesThatMustBeDoors = [n | (n,ns) <- graph d, length ns == 1]
  in case setObligatoryDoors nodesThatMustBeDoors (doorStatus d) of
    Nothing -> NoSolution -- peut également trouver une impossibilité si plus de 3 doors obligatoires
    Just newDoors -> if newDoors == (doorStatus d)
                     then NotBetter d
                     else Better d { _doorStatus = newDoors }

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
edgeOrLackOfNeighbors :: SolvingData -> SearchResult -- 'l'
edgeOrLackOfNeighbors = turnEdgeIntoPathIfNoSolutionOtherwise obviouslyNoSolution1

bouncing :: SolvingData -> SearchResult -- 'b'
bouncing = turnEdgeIntoPathIfNoSolutionOtherwise obviouslyNoSolution2

reduceGraph2 :: SolvingData -> SearchResult -- '2'
reduceGraph2 = turnEdgeIntoPathIfNoSolutionOtherwise (obviouslyNoSolution1 `onReduced` reduceStrategy "3*")

--reduceGraph2B :: SolvingData -> SearchResult
--reduceGraph2B = turnEdgeIntoPathIfNoSolutionOtherwise (obviouslyNoSolution2 `onReduced` reduceStrategy "3*")

onReduced :: (SolvingData -> Bool) -> (SolvingData -> SearchResult) -> (SolvingData -> Bool)
f `onReduced` g = \d -> case g d of
  NoSolution -> True
  NotBetter d -> f d
  Better d -> f d

obviouslyNoSolution :: SolvingData -> SearchResult -- SLOW
obviouslyNoSolution d =
  if obviouslyNoSolution1 d || obviouslyNoSolution2 d
  then NoSolution
  else NotBetter d

obviouslyNoSolution1 d = any (doesNotHaveEnoughNeighbors (doorStatus d)) (graph d)
obviouslyNoSolution2 d = any (hasOnlyTwoNeighborsThatAreTheEndsOfASinglePath (paths d)) (graph d)

doesNotHaveEnoughNeighbors :: DoorStatus -> (Node, [Node]) -> Bool
doesNotHaveEnoughNeighbors ds (n,ns) = case () of
  _ | obligatoryDoors ds `contains` n -> length ns < 1 -- si un noeud est forcément une porte a moins de 1 voisin
  _ | possibleDoors ds `contains` n -> length ns < 1 -- si un noeud qui est peut-être une porte a moins de 1 voisin
  _ -> length ns < 2 -- si un noeud qui n'est pas une porte a - de 2 voisins (usuellement déjà réduit par reduceGraph3)

hasOnlyTwoNeighborsThatAreTheEndsOfASinglePath ws (n,ns) = any (setEqual ns) $ map bothEnds ws where
  setEqual (n1:n2:[]) (f,l) = (n1 == f && n2 == l) || (n2 == f && n1 == l)
  setEqual _ (f,l) = False -- pas de solution si un noeud a seulement 2 voisins qui sont les extrémités d'un même path
