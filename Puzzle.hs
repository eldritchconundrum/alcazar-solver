module Puzzle (
  Coords,
  Size,
  validCoords,
  Puzzle(), -- hide the constructor
  size, doors, wallsDown, wallsRight,
  buildPuzzle,
  transposePuzzle
  ) where

import Data.List
import Utils

-- O--->x
-- |
-- V
-- y
type Coords = (Int, Int) -- (x,y)
type Size = (Int, Int) -- (x,y)

validCoords :: Size -> Coords -> Bool
validCoords (width, height) (x, y) = x >= 0 && y >= 0 && x < width && y < height

validSize :: Size -> Bool
validSize (width, height) = width > 0 && height > 0

data Puzzle = Puzzle { _size :: Size,
                       _doors :: [Coords],
                       _wallsDown :: [Coords],
                       _wallsRight :: [Coords] }
size = _size
doors = _doors
wallsDown = _wallsDown
wallsRight = _wallsRight

{-

buildPuzzle is the only public constructor of type Puzzle, and ensures that
every instance satisfies the following:

* every Coords is in the rectangle

* the walls are correctly placed in the rectangle

* there are no duplicate coords in the lists (they are sets)

-}

buildPuzzle :: Size -> [Coords] -> [Coords] -> [Coords] -> Puzzle
buildPuzzle sz@(w,h) ds wds wrs =
  let checkList :: String -> ([a] -> Bool) -> [a] -> [a]
      checkList message areValid xs = if areValid xs then xs
                                      else error ("incorrect puzzle definition: " ++ message)
      nodup xs = nub xs == xs
  in if not (validSize sz) then error "invalid size" else Puzzle sz
     (checkList "duplicate doors" nodup $
      checkList "invalid door coords" (all (validCoords sz)) $ ds)
     (checkList "duplicate walls-down" nodup $
      checkList "invalid wall-down coords" (all (validCoords (w,h-1))) $ wds)
     (checkList "duplicate walls-right" nodup $
      checkList "invalid wall-right coords" (all (validCoords (w-1,h))) $ wrs)

transposePuzzle :: Puzzle -> Puzzle
transposePuzzle puz = buildPuzzle
  (swap (size puz))
  (map swap (doors puz))
  (map swap (wallsRight puz))
  (map swap (wallsDown puz)) where swap (a,b) = (b,a)
