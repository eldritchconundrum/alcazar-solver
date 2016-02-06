module Graph {-( -- This file should be split and renamed  
  Node,nodeToCoords,coordsToNode,
  Path,bothEnds,inside,expandPaths,
  DoorStatus,obligatoryDoors,possibleDoors,doorPairs,updateDoors,
  Graph,
  allNodes,neighbors,
  SolvingData(), -- hide the constructor
  puzzle, doorStatus, graph, paths,
  buildInitialData, addPath, withoutEdge,
  asciiArtSolution,
  )-} where

import Data.List
import Data.Maybe (fromJust, fromMaybe)
import Utils
import Puzzle
import Doors


-- This type represents an obligatory path.
type Path = [Node]
bothEnds p = (first p, last p)
inside = init . tail

-- replace in nodeList every consecutive [start,end] of a path by the full path
expandPaths :: [Path] -> [Node] -> [Node]
expandPaths ps nodeList = let
  findOrientedPath :: Node -> [Path] -> Maybe Path
  findOrientedPath n [] = Nothing
  findOrientedPath n (w:ws) = let (f,l) = bothEnds w in case () of
    _ | n == f -> Just w
    _ | n == l -> Just (reverse w)
    _ -> findOrientedPath n ws
  expandPath [] = []
  expandPath (n:ns) = case findOrientedPath n ps of
    Nothing -> n : expandPath ns
    Just p -> if last p /= first ns
              then error "path half-followed"
              else p ++ expandPath (tail ns)
  in expandPath nodeList
--expandPaths [0,4,9,3,7,8,12,14,19] [[7,6,3],[4,5,2,1,0],[12,13,14]] == [0,1,2,5,4,9,3,6,7,8,12,13,14,19]

updatePaths :: [Node] -> [Path] -> [Path]
updatePaths [n1,n2] ws = updatePaths2 n1 n2 ws
updatePaths [n1,nw,n2] ws = updatePaths3 n1 nw n2 ws
updatePaths _ _ = error "can I has implematoin kthx"  

updatePaths2 :: Node -> Node -> [Path] -> [Path]
updatePaths2 n1 n2 ws =
  let (atMostTwoPaths, otherWs) = partition (\w -> let (f,l) = bothEnds w in length (nub [f, l, n1, n2]) < 4) ws
      -- among [pre,sep,post], at most two of them will match the (at most two) paths
      -- we only have to find who's where, and reverse and init/tail the paths to avoid duplicate n1/n2
      pathsWithReverse = atMostTwoPaths ++ map reverse atMostTwoPaths
      before = maybe [] init $ find ((\(f,l) -> f /= n2 && l == n1) . bothEnds) pathsWithReverse
      sep = maybe [] inside $ find ((\(f,l) -> f == n1 && l == n2) . bothEnds) pathsWithReverse
      after = maybe [] tail $ find ((\(f,l) -> f == n2 && l /= n1) . bothEnds) pathsWithReverse
  in (before ++ [n1] ++ sep ++ [n2] ++ after) : otherWs

updatePaths3 :: Node -> Node -> Node -> [Path] -> [Path]
updatePaths3 n1 nw n2 ws = -- [n1,nw,n2] are distinct, (flatten ws) too.
  let (atMostTwoPaths, otherWs) = partition (\w -> let (f,l) = bothEnds w in length (nub [f, l, n1, n2]) < 4) ws
      -- among [pre,sep1,sep2,post], at most two of them will match the (at most two) paths
      -- we only have to find who's where, and reverse and init/tail the paths to avoid duplicate n1/n2
      pathsWithReverse = atMostTwoPaths ++ map reverse atMostTwoPaths
      before = maybe [] init $ find ((\(f,l) -> f /= nw && l == n1) . bothEnds) pathsWithReverse
      sep1 = maybe [] inside $ find ((\(f,l) -> f == n1 && l == nw) . bothEnds) pathsWithReverse
      sep2 = maybe [] inside $ find ((\(f,l) -> f == nw && l == n2) . bothEnds) pathsWithReverse
      after_ = maybe [] tail $ find ((\(f,l) -> f == n2 && l /= nw) . bothEnds) pathsWithReverse
  in (before ++ [n1] ++ sep1 ++ [nw] ++ sep2 ++ [n2] ++ after_) : otherWs
-- updatePaths3 1 2 3 [[1,0,2]] == [1,0,2,3]
-- updatePaths3 5 4 6 [[4,3,6],[0,1,2,5]] == [[0,1,2,5,4,3,6]] car 5 disparaît du graphe mais pas 6.




-- This type is the adjacency lists of the graph that collapses obligatory paths into a single edge.
type Graph = [(Node, [Node])]

allNodes :: Graph -> [Node]
allNodes g = map fst g

neighbors :: Graph -> Node -> [Node]
neighbors g node = fromMaybe [] $ lookup node g

updateGraphForNewPath :: Path -> Graph -> Graph
updateGraphForNewPath w adjs = update adjs where
  (np1, np2) = bothEnds w
  nodesToDelete = inside w
  update [] = []
  -- delete nodes that are inside the path
  update ((n,ns):as) | nodesToDelete `contains` n = update as
  -- replace the edges between an endpoint and a deleted node by a link to the other endpoint
  update ((n,ns):as) | n == np1 = (n,replaceItems nodesToDelete np2 ns) : update as
  update ((n,ns):as) | n == np2 = (n,replaceItems nodesToDelete np1 ns) : update as
  -- delete edges between path endpoints and deleted nodes
  update ((n,ns):as) = (n, ns \\ nodesToDelete) : update as
  replaceItems olds new = nub . map replace where replace x = if olds `contains` x then new else x



-- This type augments the puzzle data with "obligatory paths" that
-- must be part of the solution and will be concatenated into it. We
-- also store what we know about doors, and the resulting graph after
-- reducing the paths into an edge going directly from an end to the
-- other end.
-- SolvingProgress? SolvingState? hmmmgrh
data SolvingData = SolvingData { _puzzle :: Puzzle,
                                 _doorStatus :: DoorStatus,
                                 _graph :: Graph,
                                 _paths :: [Path] }
puzzle = _puzzle
doorStatus = _doorStatus
graph = _graph
paths = _paths

{-

TODO: make it easily provable that every instance of Graph satisfies
the following:   

* The graph must be non-oriented (symmetric matrix).
* Doors are nodes in the graph.
* ...

-}

buildInitialData :: Puzzle -> SolvingData
buildInitialData puz = let
  (w,h) = size puz
  cToN = coordsToNode (size puz)
  trimIsolatedNodes d = d { _graph = filter (not . isolated) (graph d) } where isolated (n,ns) = empty ns
  -- nodes that are isolated in the puzzle are considered not part of the problem, and are left out of the graph.
  in trimIsolatedNodes $
     SolvingData { _puzzle = puz,
                   _graph = [(cToN (x,y), map cToN $ neighborsOf puz (x,y))
                            | y <- [0..h-1], x <- [0..w-1]],
                   _doorStatus = NoClueAboutDoors $ map cToN (doors puz),
                   _paths = [] }
neighborsOf :: Puzzle -> Coords -> [Coords]
neighborsOf puz (x,y) = let (w,h) = size puz in
  if x < 0 || y < 0 || x >= w || y >= h then [] else
    concat [if x == w - 1 || (x,y) `elem` wallsRight puz then [] else [(x+1,y)],
            if x == 0 || (x - 1,y) `elem` wallsRight puz then [] else [(x-1,y)],
            if y == h - 1 || (x,y) `elem` wallsDown puz then [] else [(x,y+1)],
            if y == 0 || (x,y - 1) `elem` wallsDown puz then [] else [(x,y-1)]]


-- TODO: find a better name 
start :: Puzzle -> SolvingData
start puz = let
  d = buildInitialData puz
  newDoorStatus = case exploitDoorParity d of
    Just ds -> ds
    Nothing -> error "no solutions because of doors parity"
  in d { _doorStatus = newDoorStatus }

puzzleParity :: SolvingData -> Parity
puzzleParity d = let
  ns = map fst (graph d)
  ev = evenCoords ((size . puzzle) d)
  od = oddCoords ((size . puzzle) d)
  in case count od ns - count ev ns of
    1 -> BothOdd
    -1 -> BothEven
    0 -> EvenAndOdd
    _ -> error "no solutions because of puzzle parity"

exploitDoorParity :: SolvingData -> Maybe DoorStatus
exploitDoorParity d = exploitDoorParity' (puzzleParity d) (doorStatus d) where
  ev = evenCoords ((size . puzzle) d)
  od = oddCoords ((size . puzzle) d)
  exploitDoorParity' BothOdd ds = updateDoors od ds
  exploitDoorParity' BothEven ds = updateDoors ev ds
  exploitDoorParity' EvenAndOdd ds@(NoClueAboutDoors ns) = let
    (odDoors, evDoors) = (filter od ns, filter ev ns)
    in updateDoors (const True) (TwoSetsOfDoorsOneInEach odDoors evDoors)
  exploitDoorParity' EvenAndOdd (TwoSetsOfDoorsOneInEach ns1 ns2) =
    -- TwoSetsOfDoorsOneInEach n'est possible qu'en tant que résultat de exploitDoorParity sur un NoClueAboutDoors.
    -- Comme il n'y a pas d'intérêt à appeler exploitDoorParity plusieurs fois, ce cas est impossible.
    error "exploitDoorParity on TwoSetsOfDoorsOneInEach should not happen"
    -- (ça tombe bien parce qu'il avait l'air compliqué)
  exploitDoorParity' EvenAndOdd (OneDoorFound n ns) = let
    keep = if od n then ev else od
    in case filter keep ns of
      [] -> Nothing
      [n2] -> Just $ TwoDoorsFound n n2
      ns' -> Just $ OneDoorFound n ns'
  exploitDoorParity' EvenAndOdd ds@(TwoDoorsFound n1 n2) =
    if (od n1 && ev n2) || (ev n1 && od n2) then Just ds else Nothing


----------------------------------------------------------------------- graph reduction by adding/merging a path

{-

Removing edges from a graph by turning them into a path means: adding
the new path and merging it with other paths if necessary; edges are
replaced by just one edge between the ends of the path; endpoints of
the reduced path may disappear inside the resulting merged path.

-}

addPath :: [Node] -> SolvingData -> Maybe SolvingData
addPath ns d =
  let g = graph d
      newGraph = updateGraphForNewPath newPath g
      newPaths@(newPath:_) = updatePaths ns (paths d)
  in if length (allNodes (graph d) `intersect` ns) /= length ns then error "bad edge" else
       case updateDoors (not . empty . neighbors newGraph) (doorStatus d) of
         Nothing -> Nothing -- Nothing means no solution because door was collapsed
         Just ds -> Just $ d { _paths = newPaths,
                               _graph = newGraph,
                               _doorStatus = ds }


withoutEdge :: SolvingData -> (Node,Node) -> SolvingData
withoutEdge d (e1,e2) = d { _graph = map deleteEdge (_graph d) } where
  deleteEdge :: (Node, [Node]) -> (Node, [Node])
  deleteEdge (e,es) | e == e1 = (e, delete e2 es)
  deleteEdge (e,es) | e == e2 = (e, delete e1 es)
  deleteEdge (e,es) = (e, es)
  -- ici faudrait ptêt error si l'edge est également un path 

----------------------------------------------------------------------- it's show time!

instance Show Puzzle where
  show puz = asciiArt (buildInitialData puz)

instance Show SolvingData where
  show d = asciiArt d ++
    (intercalate " " $ map (\(n,ns) -> show n ++ show ns) (graph d)) ++
    "\n" ++ (show . length . allNodes . graph) d ++ " nodes, " ++
    (show . length . concat . map snd . graph) d ++ " edges, doors=" ++
    show (doorStatus d) ++ " | wormholes=" ++ showList (paths d)
    where showList = init . tail . show -- trim [ and ]

asciiArt :: SolvingData -> String
asciiArt d = asciiArt' d (paths d)

asciiArtSolution :: SolvingData -> Path -> String
asciiArtSolution d p = asciiArt' d [p]

asciiArt' :: SolvingData -> [Path] -> String
asciiArt' d ps = (unlines . map concat) cellMatrix where
  g = graph d
  sz = size (puzzle d)
  (width, height) = sz
  cToN = coordsToNode sz
  nToC = nodeToCoords sz
  -- make a 2*width+1 by 2*height+1 cell matrix, cells can be separators and separator connectors
  cellMatrix :: [[String]]
  cellMatrix = [[showCell xx yy | xx <- [0..2*width]] | yy <- [0..2*height]]
  -- each cell is displayed on two chars
  showCell :: Int -> Int -> String
  showCell xx yy = let (x,y) = (xx `div` 2, yy `div` 2) in case () of
    _ | odd xx && odd yy -> showNodeCell x y -- node
    _ | even xx && even yy -> showConnectorCell x y -- connector between separators
    _ | even xx && odd yy -> showSeparatorCell (x-1,y) (x,y) -- vertical separator
    _ | odd xx && even yy -> showSeparatorCell (x,y-1) (x,y) -- horizontal separator
  showNodeCell :: Int -> Int -> String
  showNodeCell x y = case () of
    _ | cToN (x,y) `elem` obligatoryDoors (doorStatus d) -> obligatoryDoorCell
    _ | cToN (x,y) `elem` possibleDoors (doorStatus d) -> if even (x+y) then possibleDoorCell1 else possibleDoorCell2
    _ | any (`contains` cToN (x,y)) ps  -> pathCell
    _ | not (allNodes (graph d) `contains` cToN (x,y)) -> filledCell -- not in the graph, show fully filled
    _ -> emptyCell
  showConnectorCell x y = filledCell
  showSeparatorCell c1 c2 =
    if not $ all (validCoords sz) [c1,c2] then filledCell else -- border walls
      let (n1, n2) = (cToN c1, cToN c2) in
      if areAdjacentInAPath ps [n1, n2] then pathCell else
        -- use the puzzle, not the graph (which may have additional edges for paths)
        if neighborsOf (puzzle d) (nToC n2) `contains` (nToC n1)
        then if neighbors (graph d) n1 `contains` n2
             then emptyCell
             else deducedFilledCell -- eliminated by paths or by removeEdges
        else filledCell
  -- TODO ascii art de folie "▒░▓█" 🐱 │──┼║══╬┃━━┼ │┃━─┼█▓▒░ ——·•⚫●⬤✝+✚✛➕❚⍭⎢⎮⎺⎻⎼⎽
  -- http://www.fileformat.info/info/unicode/block/box_drawing/list.htm -- "─│┌┐└┘├┤┬┴┼"
  -- http://www.fileformat.info/info/unicode/block/block_elements/list.htm
  (filledCell, emptyCell, pathCell, deducedFilledCell) = ("▒▒", "  ", "░░", "▓▓")
  -- http://www.fileformat.info/info/unicode/block/miscellaneous_symbols_and_pictographs/list.htm
  (possibleDoorCell1, possibleDoorCell2, obligatoryDoorCell) = ("🐱 ", "🐵 ", "✅ ") -- "🐮 "

areAdjacentInAPath :: [Path] -> [Node] -> Bool
areAdjacentInAPath ps cs = any (cs `isInfixOf`) ps || any ((reverse cs) `isInfixOf`) ps
