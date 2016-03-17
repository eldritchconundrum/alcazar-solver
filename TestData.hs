module TestData where
import Puzzle

allPuzzles = [
  --noSolutionPuz,
  multipleSolutionPuz,
  smallestPuz, threeSquaresPuz, trivialPuz, simpleAsymmetricPuz, zorro1Puz, zorro2Puz,
  provablyUselessDoorPuz, fourPuz, my4x4Puz, fivePuz,
  sixPuz,
  transposePuzzle (multiple4x4Rooms 2), transposePuzzle (multiple4x4Rooms 4), multiple4x4Rooms 6,
  transposePuzzle (multiple4x4RoomsWithDoors 2), transposePuzzle (multiple4x4RoomsWithDoors 4),
  multiple4x4RoomsWithDoors 6,
  jsPuz, daily6x9Puz, sixRoomsPuz, excellentHandMadePuz, book10x15Puz, hardBook10x15Puz,
  valentinePuz, daily7x10Puz, manyDoors, dailyOther7x10, daily5x8, mars4Puz,
  unitTestReduce3RemoveEdges,
  feb13, mar14, mar17
  ]

noSolutionPuz = buildPuzzle (2,2) [(1,0),(1,1)] [(1,0)] [(0,1)]
multipleSolutionPuz = buildPuzzle (3,3) [(0,0),(2,2)] [] []

instantaneousPuzzles = [smallestPuz, threeSquaresPuz, trivialPuz, simpleAsymmetricPuz, zorro1Puz, zorro2Puz,
                        provablyUselessDoorPuz, fourPuz, my4x4Puz, fivePuz
                        --sixPuz, transposePuzzle (multiple4x4Rooms 2)
                       ]
smallestPuz = buildPuzzle (2,1) [(0,0),(1,0)] [] []
threeSquaresPuz = buildPuzzle (2,2) [(0,0),(1,1)] [(1,0)] [(0,0)]
trivialPuz = buildPuzzle (2,2) [(1,0),(1,1)] [(1,0)] []
simpleAsymmetricPuz = buildPuzzle (4,2) [(0,0),(3,0)] [] [(2,0)]
zorro1Puz = buildPuzzle (3,3) [(0,0),(2,2)] [(2,1)] []
zorro2Puz = buildPuzzle (3,3) [(0,0),(2,2)] [(0,0),(1,0),(1,1),(2,1)] []
provablyUselessDoorPuz = buildPuzzle (3,3) [(0,0),(2,0),(1,2)] [] [(0,0)]
fourPuz = buildPuzzle (4,4) [(0,1),(2,0),(3,2)] [(2,0),(2,1),(1,1)] [(2,2)]
my4x4Puz = buildPuzzle (4,4) [(0,0),(0,1),(1,0),(2,3)] [(1,1)] [(1,1),(1,3)]
fivePuz = buildPuzzle (5,5) [(2,0),(0,2),(2,4),(4,2)] [(3,0),(1,1),(2,2),(3,2)] [(2,1),(1,2),(1,3)]
sixPuz = buildPuzzle (6,6) [(5,5),(5,2)] [(2,4),(4,2),(4,3),(3,1)] [(0,1),(2,1),(1,3),(2,4),(4,5)]
multiple4x4Rooms n = buildPuzzle (4,4*n) [(1,0),(1,4*n-1)] (concat [[(0,4*i-1),(3,4*i-1)] | i <- [1..n-1]]) []
multiple4x4RoomsWithDoors n = buildPuzzle (4,4*n) ([(1,0),(1,4*n-1)] ++ [(1,i) | i <- [3..4*n-4]])
                                  (concat [[(0,4*i-1),(2,4*i-1),(3,4*i-1)] | i <- [1..n-1]]) []

slowerPuzzles = [jsPuz, {-daily6x9Puz, sixRoomsPuz, excellentHandMadePuz,-}
                 transposePuzzle (multiple4x4RoomsWithDoors 3)]
jsPuz = buildPuzzle (6,7) [(0,3),(5,3)] [(1,1)] [(3,1),(2,2),(1,3),(3,3)]
daily6x9Puz = buildPuzzle (6,9) [(1,0),(4,0),(0,7),(5,7),(4,8)]
              [(2,1),(3,1),(1,6),(4,4)] [(2,0),(1,3),(1,4),(3,3),(3,4),(1,6)]
sixRoomsPuz = buildPuzzle (8,12) [(0,1), (7,1), (0,6), (7,6), (0,9), (7,9)]
              [(2,1), (5,1), (0,3),(2,3),(3,3),(4,3),(7,3), (5,5), (0,7),(3,7),(4,7),(5,7),(6,7),(7,7), (1,9)]
              ([(5,10)] ++ [(3,y) | y <- [0,1,3,4,5,7,8,11]])

excellentHandMadePuz = -- http://www.theincrediblecompany.com/2014/07/19/an-excellent-hand-made-alcazar/
  buildPuzzle (10,10) [(0,0),(0,9),(9,0),(9,9)]
  [(1,3),(2,3),(2,4),(1,5),(2,5),(7,0),(8,0),(7,2),(8,2),(7,6),(8,6),(7,8),(8,8)]
  [(0,1),(0,2),(1,1),(1,2),(2,2),(0,8),(1,8),(1,7),(2,8),(2,7),(3,1),(3,2),(3,3),
   (3,4),(3,5),(3,6),(3,7),(3,8),(4,1),(4,2),(4,3),(4,6),(4,7),(4,8),(5,1),(5,2),
   (5,3),(5,4),(5,5),(5,6),(5,7),(5,8),(7,4),(7,5),(8,5)]

book10x15Puz = -- large but reduces very well
  buildPuzzle (10,15) [(2,0),(5,0),(9,3),(9,4),(0,11),(1,14),(7,14),(9,12)]
  [(1,1),(2,1),(3,0),(6,0),(5,2),(6,3),(7,3),(8,3),(9,3),(1,4),(6,5),(1,6),
   (2,7),(4,8),(7,8),(9,8),(0,9),(1,10),(2,10),(3,10),(4,10),(6,10),(5,11),(2,12),(3,12),(1,13)]
  [(0,2),(0,5),(2,3),(2,5),(2,6),(2,9),(3,1),(3,2),(4,4),(4,5),(4,7),(6,1),
   (7,2),(7,5),(7,7),(6,6),(5,8),(5,9),(7,9),(6,10),(5,11),(7,11),(7,13),(8,12),(5,14),(5,13),(6,12)]

hardBook10x15Puz = -- the same problem but more difficult, with the same solution, still unique
  buildPuzzle (10,15) [(3,0),(4,0),(6,0),(7,0),(8,0),(9,0),(9,1),(9,2),(9,5),(9,7),(9,9),(9,10),(9,11),(9,13),(8,14),(6,14),(5,14),(4,14),(2,14),(0,13),(0,3),(0,1),(0,10),(0,12),(0,8),(0,9),(0,4),(0,5),(0,6),(2,0),(5,0),(9,3),(9,4),(0,11),(1,14),(7,14),(9,12)] [(1,1),(3,0),(6,0),(5,2),(7,3),(9,3),(1,4),(6,5),(1,6),(2,7),(7,8),(9,8),(0,9),(1,10),(3,10),(6,10),(5,11),(2,12),(3,12),(1,13)] [(0,2),(0,5),(2,3),(2,6),(3,1),(4,4),(4,7),(6,1),(7,2),(7,5),(7,7),(6,6),(6,10),(5,11),(7,11),(7,13),(8,12),(5,13)]


valentinePuz = -- https://twitter.com/mhuovila/status/566769283187748864
  buildPuzzle (12,10) [(0,2),(0,4),(0,5),(2,7),(3,0),(4,1),(5,9),(8,0),(9,0),(9,7),(10,1),(11,2),(11,4),(11,5)] wd wr where
    out = [(0,0),(1,0),(4,0),(5,0),(6,0),(7,0),(10,0),(11,0),
           (0,1),(5,1),(6,1),(11,1)]
    wd = [(x,y) | x <- [0..3], y <- [5..8], x + 5 <= y] ++
         [(x,y) | x <- [8..11], y <- [5..8], x > 15 - y] ++
         [(0,0),(1,0),(4,0),(5,0),(6,0),(7,0),(10,0),(11,0),(0,1),(5,1),(6,1),(11,1)] ++
         [(2,3),(4,3),(6,3),(8,3),(11,3),(7,5),(9,5),(9,6),(3,6)]
    wr = [(x,y) | x <- [0..3], y <- [5..9], x + 6 <= y] ++
         [(x,y) | x <- [7..10], y <- [5..9], x > 15 - y] ++
         [(0,0),(1,0),(3,0),(4,0),(5,0),(6,0),(7,0),(9,0),(10,0),(0,1),(4,1),(5,1),(6,1),(10,1)] ++
         [(1,4),(4,4),(4,7),(4,8),(5,3),(8,1),(6,6),(5,9)]
valentineSol = [(2,7),(2,6),(1,6),(1,5),(0,5),(0,4),(1,4),(1,3),(0,3),(0,2),(1,2),(1,1),(2,1),(2,0),(3,0),(3,1),
                (4,1),(4,2),(3,2),(2,2),(2,3),(3,3),(4,3),(5,3),(5,2),(6,2),(6,3),(7,3),(8,3),(9,3),(9,2),(8,2),
                (7,2),(7,1),(8,1),(8,0),(9,0),(9,1),(10,1),(10,2),(11,2),(11,3),(10,3),(10,4),(11,4),(11,5),(10,5),
                (10,6),(9,6),(8,6),(7,6),(7,7),(6,7),(5,7),(5,6),(6,6),(6,5),(7,5),(8,5),(9,5),(9,4),(8,4),(7,4),
                (6,4),(5,4),(5,5),(4,5),(4,4),(3,4),(2,4),(2,5),(3,5),(3,6),(4,6),(4,7),(3,7),(3,8),(4,8),(4,9),
                (5,9),(5,8),(6,8),(6,9),(7,9),(7,8),(8,8),(8,7),(9,7)]

daily7x10Puz = buildPuzzle (7,10) [(2,0), (6,3), (6,5), (6,7), (2,9), (4,9)] [(1,4), (4,3), (6,1), (6,7), (2,7), (1,6)]
               [(2,1),(1,2),(4,3),(1,4),(3,4),(4,5),(1,7)]
manyDoors = buildPuzzle (7,10) [(0,0),(1,0),(2,0),(5,0),(6,1),(6,3),(6,5),(6,6),(6,7),(6,8),
                               (0,2),(0,4),(0,6),(0,8),(1,9),(2,9),(3,9),(4,9),(5,9),(6,9)]
                [(2,1),(4,3),(2,5),(3,7),(4,8)] [(1,2),(1,3),(1,5),(1,6),(3,4),(4,3),(4,2),(3,8),(5,7)]
dailyOther7x10 = buildPuzzle (7,10) [(1,0),(3,0),(5,0),(0,1),(6,1),(0,3),(6,3),(0,5),(6,5),(0,7),(6,7),(1,9),(4,9)]
                 [(2,1),(0,3),(3,2),(4,3),(6,1),(2,5),(4,6),(5,7)] [(1,1),(1,3),(0,7),(2,6),(3,3),(4,6),(4,7)]
daily5x8 = buildPuzzle (5,8)
           [(1,0),(2,0),(3,0),(0,1),(0,2),(0,4),(0,5),(0,6),(1,7),(3,7),(4,7),(4,6),(4,5),(4,4),(4,3),(4,2)]
           [(0,2),(1,1),(4,2)] -- raisonnement sur les doors et la parité et des sous-parties
           [(2,5),(2,6),(2,7)]

mars4Puz = buildPuzzle (7,10) [(0,0),(4,0),(5,0),(6,1),(0,2),(6,3),(0,4),(6,5),(6,7),(2,9),(4,9)]
           [(3,1),(3,4),(4,3),(0,5),(5,5),(4,7)]
           [(1,2),(1,4),(4,3),(3,4),(1,7),(1,8),(4,8)]

unitTestReduce3RemoveEdges = buildPuzzle (16,16) [(0,1),(15,7)] [] $
                             [(x,y) | x <- [1,3..13], y <- [2..14]] ++
                             [(x,y) | x <- [1,3..13], y <- if x `mod` 4 == 1 then [0,1] else [15]]

feb13 = buildPuzzle (7,10) (concat [[(0,i),(i,0),(6,i)] | i <- [1,3,5]] ++ [(0,7),(6,7),(0,9),(2,9),(5,9)])
        [(3,1),(4,1),(5,2),(6,2),(2,5),(3,5)] [(4,2),(1,3),(2,3),(2,5),(2,6),(2,7),(1,8),(1,9),(4,7)]

mar14 = buildPuzzle (7,10) [(0,0),(2,0),(4,0),(0,2),(0,4),(0,6),(0,8),(1,9),(3,9),(5,9),(6,8),(6,5),(6,3)]
        [(0,3),(0,5),(2,3),(4,0),(4,2),(6,6)] [(0,8),(1,1),(1,7),(2,2),(2,8),(3,6),(4,1),(4,3)]

mar17 = buildPuzzle (7,10) [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,1),(6,3),(6,4),(6,5),(6,6),(6,8),(5,9),(3,9),(1,9),(0,8),(0,7),(0,5),(0,3),(0,2),(0,1)]
        [(2,1),(1,2),(3,5),(0,6),(1,6),(5,5),(6,5),(6,7),(4,7),(2,7)] [(1,2),(4,2),(1,4),(3,5),(1,6)]

-- size doors wallsDown wallsRight
-- "1 13".split.map(&:to_i).each_slice(2) { |n,m| print "(#{n},#{m})," }; puts; puts


{-

http://edderiofer.blogspot.fr/2014/11/parity-in-alcazar-and-other-such-loop.html

https://github.com/sjb3d/alcazam
+   +---+   +---+---+---+---+---+---+   +
|                               |       |
+   +   +   +   +---+   +   +   +   +   +
|                                       |
+   +   +---+---+---+---+   +   +   +   +
        |       |       |               |
+   +   +   +   +   +   +   +   +   +   +
|                                       |
+   +   +---+   +   +   +   +---+   +   +
|                               |       |
+   +   +---+   +   +   +   +   +   +   +
|                                       |
+---+   +   +   +---+   +   +---+   +   +
|               |           |           |
+   +   +   +   +   +   +   +   +   +   +
|       |                               |
+   +   +   +   +   +   +   +   +   +   +
|                                   |   |
+   +   +   +   +   +   +   +   +   +   +
|               |           |           |
+---+---+   +---+---+   +---+---+---+   +


http://david-westreicher.github.io/2014/11/06/alcasat/
xxxxxxxxxxxxxxxxxxxxxxxxx
x0-0x0-0-0-0-0-0-0-0x0-0x
x|x|x|xxx x x x x x|x|x|x
x0 0x0-0-0-0x0-0-0 0x0 0x
x|x|x xxxxx|x|x x|x|x|x|x
x0 0-0 0-0 0 0x0-0x0-0x0 
x|xxx|x|x|x|x|x|xxx x x|x
x0 0-0 0x0x0-0 0-0-0-0x0x
x|x|xxx|x|xxx xxx xxx|x|x
 0x0 0-0 0 0-0-0 0-0-0 0x
x|x|x|xxx|x|xxx|x|x xxx|x
x0x0-0 0-0x0-0 0x0-0-0x0x
x|xxx x|x x x|x|xxx x|x|x
x0-0-0x0-0-0 0x0x0-0x0 0x
x xxx|xxx x|x|x|x|x|x|x|x
x0-0x0-0x0-0x0x0-0x0-0x0 
x|x|xxx|x|x x|x x x xxx|x
x0 0-0-0 0-0x0-0-0-0-0x0x
x|xxxxx xxx|x xxx x x|x|x
x0-0 0-0-0-0x0-0-0x0-0x0x
x x|x|xxx xxx|x x|x|xxx|x
x0-0x0-0-0-0x0 0-0x0-0 0x
x|x xxx xxx|x|x|x x x|x|x
x0-0-0-0-0 0-0x0-0-0x0x0x
x xxx xxx|x x x xxx|x|x|x
x0-0-0-0-0x0-0x0-0x0x0 0 
x|x xxx x x|x|x|x|x|x|x|x
x0-0-0-0-0x0x0-0 0-0 0x0-
x xxx xxx|x|x xxx xxx|xxx
x0-0-0-0-0 0x0-0-0 0-0 0-
x|xxx x xxx|x|x x|x|xxx|x
x0 0-0x0-0 0-0x0-0x0 0-0x
x|x|x|x|x|x xxx|x x|x|x x
x0 0x0 0x0x0-0x0-0-0x0-0x
x|x|x|x|x|x|x|x x xxx x|x
x0-0 0-0 0-0 0-0-0-0-0-0x
xxxxxxxxxxxxxxxxxxxxxxxxx

xxx x xxx x|xxxxx
x0-0x0-0-0x0-0-0x
x|x|x|x x|x x x|x
x0 0-0x0-0x0-0x0 
x|xxx x|x x|x|x|x
x0 0-0x0 0-0x0 0x
x|x|x|x|x|xxx|x|x
 0-0 0 0x0 0-0 0x
x xxx|x|x|x|xxx|x
x0-0-0 0-0x0-0-0x
x|xxxxx xxx xxx x
x0-0-0-0x0-0-0-0x
x x x x|x|xxxxx|x
x0-0-0-0 0-0 0-0x
x|xxxxxxxxx|x|x x
x0-0-0 0-0 0x0-0-
xxx x|x|x|x|x xxx
x0-0 0-0 0x0-0-0x
x|x|xxxxx|x xxx|x
 0x0-0-0-0 0-0 0x
x|x x xxx x|x|x|x
x0-0-0-0-0-0 0-0x
xxxxxxxxxxxxxxxxx

https://pbs.twimg.com/media/CAtS1yPWgAA7jtY.png:large

-}
