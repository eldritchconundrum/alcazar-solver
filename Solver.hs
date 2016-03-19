
{-# OPTIONS_GHC
  -Wall
  -fno-warn-orphans
  -fno-warn-name-shadowing
  -fno-warn-unused-binds
  -fno-warn-missing-signatures
  -fno-warn-unused-matches
  -fno-warn-incomplete-patterns
#-}

module Solver {-(bruteForce, reduce, Solving(..))-} where

import Data.List
import Data.Maybe
import Utils
--import Puzzle
import Graph
import Doors
import Strategy
import Control.Monad

bruteForce :: SolvingData -> [[Node]]
bruteForce d = map (_expandPaths (paths d)) $ cleanUpSolutions $ _bruteForceImpl d where
  cleanUpSolutions :: Ord a => [[a]] -> [[a]]
  cleanUpSolutions paths = nub [if first path < last path then path else reverse path | path <- paths]

-- replace in nodeList every consecutive [start,end] of a path by the full path. start must be followed by end. all paths must be used.
-- _expandPaths [[7,6,3],[4,5,2,1,0],[12,13,14]] [0,4,9,3,7,8,12,14,19] == [0,1,2,5,4,9,3,6,7,8,12,13,14,19]
_expandPaths :: [Path] -> [Node] -> [Node]
_expandPaths [] ns = ns
_expandPaths ps (n:n':ns) = case _findOrientedPath n ps of
  Nothing -> n : _expandPaths ps (n':ns)
  Just p -> if last p /= n'
            then error "path not followed"
            else p ++ _expandPaths (delete p . delete (reverse p) $ ps) ns
_expandPaths ps _ = error $ "unused path!? " ++ show ps

_findOrientedPath :: Node -> [Path] -> Maybe Path
_findOrientedPath n [] = Nothing
_findOrientedPath n (p:ps) | n == f    = Just p
                           | n == l    = Just (reverse p)
                           | otherwise = _findOrientedPath n ps
  where (f,l) = bothEnds p

_bruteForceImpl :: SolvingData -> [[Node]]
_bruteForceImpl d = concat [dfs d2 [d1] d1 ((delete d1 . allNodes . graph) d) compiledPaths
                           | (d1,d2) <- doorPairs (doorStatus d)] where
  compiledPaths :: [Edge]
  compiledPaths = map bothEnds (paths d)
  findPathAndOtherEnd :: [Edge] -> Node -> Maybe (Edge, Node)
  findPathAndOtherEnd [] node = Nothing -- it's a little faster to have paths compiled into their (Node,Node) ends
  findPathAndOtherEnd (p@(f,l):ps) node | node == f = Just (p,l)
                                        | node == l = Just (p,f)
                                        | otherwise = findPathAndOtherEnd ps node
  dfs :: Node -> [Node] -> Node -> [Node] -> [Edge] -> [[Node]]
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
  Better d' -> converge search d'
  d' -> d'

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
  extraEdgesOfObligatoryDoorsInPath = [(n, ns \\ [n']) |
                                       (n,ns) <- (graph d),
                                       obligDoors `contains` n,
                                       n' <- maybeToList (_findOtherEnd n (paths d))]
  reductions = [flip withoutEdge (n,n') | (n,ns) <- extraEdgesOfObligatoryDoorsInPath, n' <- ns]
  in if empty reductions then NotBetter d else Better $ foldr (.) id reductions d

_findOtherEnd :: Node -> [Path] -> Maybe Node
_findOtherEnd n ps = foldM otherEnd n ps where
  otherEnd n p | f == n = Just l
               | l == n = Just f
               | otherwise = Nothing
    where (f,l) = bothEnds p

-------------------------------------------------- trouve les paths de longeur 3. rapide.

reduceGraph3 :: SolvingData -> SearchResult
reduceGraph3 d = _firstReduction d reductions where
  reductions = [reduce3NodesIntoPath (n,ns) |
                (n,ns) <- graph d,
                not (possibleDoors (doorStatus d) `contains` n) && length ns == 2]
  reduce3NodesIntoPath (nw,[n1,n2]) = addPath [n1, nw, n2] d

_firstReduction :: SolvingData -> [Maybe SolvingData] -> SearchResult
_firstReduction d [] = NotBetter d
_firstReduction d (Nothing:_) = NoSolution
_firstReduction d ((Just d'):_) = Better d'

-------------------------------------------------- trouve des paths de longeur 2. lent.

turnEdgeIntoPathIfNoSolutionOtherwise :: (SolvingData -> Bool) -> SolvingData -> SearchResult
turnEdgeIntoPathIfNoSolutionOtherwise isImpossible d = _firstReduction d reducedGraphs where
  reducedGraphs = [addEdgePath edge d |
                   edge <- everyNonPathEdge d,
                   isImpossible (withoutEdge d edge)]
  everyNonPathEdge d =
    [(e1,e2) |
     (e1,es) <- graph d,
     e2 <- es,
     e1 < e2, -- only process one of the two symmetric edges
     not (any (\p -> p `contains` e1 && p `contains` e2) (paths d))]
  addEdgePath (n1,n2) = addPath [n1,n2]

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
f `onReduced` g = maybe True f . toMaybe . g

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

hasOnlyTwoNeighborsThatAreTheEndsOfASinglePath ps (n,ns) = any (setEqual ns) $ map bothEnds ps where
  setEqual [n1,n2] (f,l) = (n1 == f && n2 == l) || (n2 == f && n1 == l)
  setEqual _ (f,l) = False -- pas de solution si un noeud a seulement 2 voisins qui sont les extrémités d'un même path
