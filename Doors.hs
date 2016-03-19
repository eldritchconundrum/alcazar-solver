module Doors (
  Node, nodeToCoords, coordsToNode,
  DoorStatus(),
  initialDoorStatus,obligatoryDoors,possibleDoors,doorPairs,setObligatoryDoors,updateDoors,exploitPuzzleParity,
  DoorParity(..),evenCoords,oddCoords,
  ) where
import Data.List
import Data.Maybe (fromJust, fromMaybe)
import Utils
import Puzzle
import Control.Monad

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
data DoorStatus = DoorStatus [Node] [Node] deriving Eq -- lists should be non-empty

instance Show DoorStatus where
  show (DoorStatus ns1 ns2) | ns1 == ns2 = show ns1
  show (DoorStatus ns1 ns2) = show2 ns1 ++ "-" ++ show2 ns2 where
    show2 [n] = show n
    show2 ns = show ns

initialDoorStatus ns = DoorStatus ns ns

obligatoryDoors :: DoorStatus -> [Node]
obligatoryDoors (DoorStatus ns1 ns2) = o ns1 ++ o ns2 where o [n] = [n]
                                                            o ns  = []

possibleDoors :: DoorStatus -> [Node]
possibleDoors (DoorStatus ns1 ns2) = nub (ns1 ++ ns2)

doorPairs :: DoorStatus -> [(Node, Node)]
doorPairs (DoorStatus ns1 ns2) = [(n1,n2) | n1 <- ns1, n2 <- ns2, n1 /= n2]

setObligatoryDoors :: [Node] -> DoorStatus -> Maybe DoorStatus -- Nothing means there is no solution
setObligatoryDoors ns ds = foldM setObligatoryDoor ds ns where
  setObligatoryDoor :: DoorStatus -> Node -> Maybe DoorStatus
  setObligatoryDoor ds@(DoorStatus [n1] [n2]) n' = if n1 == n' || n2 == n' then Just ds else Nothing
  setObligatoryDoor ds@(DoorStatus [n] ns) n' = oneDoorKnown n' n ns ds
  setObligatoryDoor ds@(DoorStatus ns [n]) n' = oneDoorKnown n' n ns ds
  setObligatoryDoor (DoorStatus ns1 ns2) n' = case (ns1 `contains` n', ns2 `contains` n') of
    (True, True) ->  Just $ DoorStatus [n'] (delete n' $ nub (ns1 ++ ns2))
    (True, False) -> Just $ DoorStatus [n'] ns2
    (False, True) -> Just $ DoorStatus [n'] ns1
    (False, False) -> Nothing
  oneDoorKnown n' n ns ds =
    if n == n' then Just ds
    else if ns `contains` n' then Just $ DoorStatus [n] [n']
         else Nothing

updateDoors :: (Node -> Bool) -> DoorStatus -> Maybe DoorStatus -- Nothing means there is no solution
updateDoors keepCond (DoorStatus ns1 ns2) = if length ns1' * length ns2' == 0
                                            then Nothing
                                            else Just (DoorStatus ns1' ns2')
  where ns1' = filter keepCond ns1
        ns2' = filter keepCond ns2

-- Parity of the doors of a puzzle
data DoorParity = EvenAndOdd
                | BothEven
                | BothOdd deriving Show

evenCoords :: Size -> Node -> Bool
evenCoords sz n = let (x,y) = nodeToCoords sz n in even (x + y)
oddCoords :: Size -> Node -> Bool
oddCoords sz n = let (x,y) = nodeToCoords sz n in odd (x + y)

exploitPuzzleParity :: Size -> DoorParity -> [Node] -> Maybe DoorStatus
exploitPuzzleParity sz BothOdd ns = updateDoors (oddCoords sz) (DoorStatus ns ns)
exploitPuzzleParity sz BothEven ns = updateDoors (evenCoords sz) (DoorStatus ns ns)
exploitPuzzleParity sz EvenAndOdd ns = updateDoors (const True) (DoorStatus odDoors evDoors) where
  odDoors = filter (oddCoords sz) ns
  evDoors = filter (evenCoords sz) ns
