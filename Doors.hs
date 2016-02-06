module Doors where
import Data.List
import Data.Maybe (fromJust, fromMaybe)
import Utils
import Puzzle


-- The nodes of the graph are represented as integers that map to
-- their coordinates in the rectangle.
type Node = Int

--  O--->x
--  | 0 1
--  V 2 3
--  y

nodeToCoords :: Size -> Node -> Coords
nodeToCoords (width, _) n = (n `mod` width, n `div` width)

coordsToNode :: Size -> Coords -> Node
coordsToNode (width, _) (x, y) = x + width * y


-- This type represents what we know about the doors in the solution.
data DoorStatus = NoClueAboutDoors [Node]  -- list has more than 1 item
                | TwoSetsOfDoorsOneInEach [Node] [Node] -- lists have more than 1 item
                | OneDoorFound Node [Node] -- list has more than 1 item
                | TwoDoorsFound Node Node deriving Eq

instance Show DoorStatus where
  show (NoClueAboutDoors ns) = show ns
  show (TwoSetsOfDoorsOneInEach ns1 ns2) = show ns1 ++ "-" ++ show ns2
  show (OneDoorFound n ns) = show n ++ "-" ++ show ns
  show (TwoDoorsFound n1 n2) = show n1 ++ "-" ++ show n2

obligatoryDoors :: DoorStatus -> [Node]
obligatoryDoors (NoClueAboutDoors ns) = []
obligatoryDoors (TwoSetsOfDoorsOneInEach ns1 ns2) = []
obligatoryDoors (OneDoorFound n ns) = [n]
obligatoryDoors (TwoDoorsFound n1 n2) = [n1,n2]

possibleDoors :: DoorStatus -> [Node]
possibleDoors (NoClueAboutDoors ns) = ns
possibleDoors (TwoSetsOfDoorsOneInEach ns1 ns2) = ns1 ++ ns2
possibleDoors (OneDoorFound n ns) = n : ns
possibleDoors (TwoDoorsFound n1 n2) = [n1,n2]

doorPairs :: DoorStatus -> [(Node, Node)]
doorPairs (NoClueAboutDoors ns) = [(n1,n2) | n1 <- ns, n2 <- ns]
doorPairs (TwoSetsOfDoorsOneInEach ns1 ns2) = [(n1,n2) | n1 <- ns1, n2 <- ns2]
doorPairs (OneDoorFound n ns) = [(n,n2) | n2 <- ns]
doorPairs (TwoDoorsFound n1 n2) = [(n1,n2)]

setObligatoryDoors :: [Node] -> DoorStatus -> Maybe DoorStatus
setObligatoryDoors ns ds = foldr f (Just ds) ns where f n Nothing = Nothing
                                                      f n (Just ds) = setObligatoryDoor n ds


setObligatoryDoor :: Node -> DoorStatus -> Maybe DoorStatus
setObligatoryDoor n' (NoClueAboutDoors ns) =
  if ns `contains` n'
  then Just $ OneDoorFound n' (delete n' ns)
  else Nothing
setObligatoryDoor n' (TwoSetsOfDoorsOneInEach ns1 ns2) = case (ns1 `contains` n', ns2 `contains` n') of
  (True, True) ->  Just $ OneDoorFound n' (delete n' (ns1 ++ ns2)) -- à voir, ce cas n'est pas possible ? pas d'intersection entre les deux si c'est issu de la parité... la signification de TwoSetsOfDoorsOneInEach est à préciser 
  (True, False) -> Just $ OneDoorFound n' ns2
  (False, True) -> Just $ OneDoorFound n' ns1
  (False, False) -> Nothing
setObligatoryDoor n' ds@(OneDoorFound n ns) =
  if n == n' then Just ds
  else if ns `contains` n' then Just $ TwoDoorsFound n n'
       else Nothing
setObligatoryDoor n' ds@(TwoDoorsFound n1 n2) = if n1 == n' || n2 == n' then Just ds else Nothing


updateDoors :: (Node -> Bool) -> DoorStatus -> Maybe DoorStatus -- Nothing means there is no solution
updateDoors keepCond (NoClueAboutDoors ns) = case filter keepCond ns of
  [] -> Nothing
  [n] -> Nothing
  [n1,n2] -> Just $ TwoDoorsFound n1 n2
  ns' -> Just $ NoClueAboutDoors ns'
updateDoors keepCond (TwoSetsOfDoorsOneInEach ns1 ns2) = case (filter keepCond ns1, filter keepCond ns2) of
  ([], _) -> Nothing
  (_, []) -> Nothing
  ([n1], [n2]) -> Just $ TwoDoorsFound n1 n2
  ([n1], ns2) -> Just $ OneDoorFound n1 ns2
  (ns1, [n2]) -> Just $ OneDoorFound n2 ns1
  (ns1, ns2) -> Just $ TwoSetsOfDoorsOneInEach ns1 ns2
updateDoors keepCond (OneDoorFound n ns) = if not (keepCond n) then Nothing else case filter keepCond ns of
  [] -> Nothing
  [n2] -> Just $ TwoDoorsFound n n2
  ns' -> Just $ OneDoorFound n ns'
updateDoors keepCond (TwoDoorsFound n1 n2) = if keepCond n1 && keepCond n2 then Just $ TwoDoorsFound n1 n2 else Nothing


data Parity = EvenAndOdd
            | BothEven
            | BothOdd deriving Show

evenCoords :: Size -> Node -> Bool
evenCoords sz n = let (x,y) = nodeToCoords sz n in even (x + y)
oddCoords :: Size -> Node -> Bool
oddCoords sz n = let (x,y) = nodeToCoords sz n in odd (x + y)
