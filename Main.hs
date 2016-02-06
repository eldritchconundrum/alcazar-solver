import Data.List
import Utils
import Puzzle
import Graph
import Solver
import TestData

-- rm -f *.hi && ghci Main.hs
-- rm -f *.hi *.o && ghc -O Main.hs && time ./Main

sameReductions :: (Strategy, Strategy) -> Puzzle -> Bool
sameReductions (sg1, sg2) puz = let
  d1 = (extract . reduce sg1 . start) puz
  d2 = (extract . reduce sg2 . start) puz
  in paths d1 == paths d2 && doorStatus d1 == doorStatus d2 && graph d1 == graph d2

assertMsgIO s b = if b then return () else error s
assertMsg s b = if b then id else error s

tests = do
  assertMsgIO "no sols" $ 0 == (length . bruteForce . buildInitialData) noSolutionPuz
  assertMsgIO "trivial" $ 1 == (length . bruteForce . buildInitialData) trivialPuz
  assertMsgIO "multi  " $ 2 == (length . bruteForce . buildInitialData) multipleSolutionPuz
  assertMsgIO "deduceDoorStatus" $ not $ sameReductions ("3*", "3*d") fivePuz
  assertMsgIO "lackOfNeighbors" $ not $ sameReductions ("3*", "3*l") provablyUselessEndpointPuz
  assertMsgIO "bouncing" $ not $ sameReductions ("3*", "3*b") jsPuz
  assertMsgIO "bouncing2" $ sameReductions ("3*bb", "3*b*") jsPuz
  assertMsgIO "big" $ 1 == (length . bruteForce . extract . reduce "3*23*" . start) unitTestReduce3RemoveEdges

m = main
main = do
  tests
  mapM_ (solveAndPrint "3*") instantaneousPuzzles
  printReduced "3*d3*23*23*23*23*" valentinePuz --solveAndPrint "3*d3*" valentinePuz
  printReduced "3*d3*b*e3*b*23*23*23*" daily6x9Puz -- solveAndPrint "3*d3*b*e3*b*" daily6x9Puz -- js: 23m
  solveAndPrint "3*b*3*b22223*23*b" daily7x10Puz  --printReduced "3*b*3*b" daily7x10Puz
  printReduced "3*e*3*2323*23*23*2223*23*23*23*23*23*23*23*" dailyOther7x10 --printReduced "3*e*3*" dailyOther7x10
  printReduced "3*b*3*22222222223*23*d3*dl" manyEndpoints -- printReduced "3*b*3*" manyEndpoints -- js: même pas en rêve
  printReduced "3*d3*23*223*23*23*23*222222223*23*23*" book10x15Puz -- printReduced "3*d3*" book10x15Puz -- js: même pas en rêve
  --solveAndPrint "3*b*2*" sixRoomsPuz -- js: >46h
  --solveAndPrint "3*b*223*2223*23*23*" jsPuz -- js: 0s
  --solveAndPrint "3*2*" excellentHandMadePuz -- js: 21h

printReduced strat puz = timeIO "reduced in" $ (print . reduce strat . start) puz
solveAndPrint strat puz = do
  reduced <- time "reduced in" $ (toMaybe . reduce strat . start) puz
  case reduced of
    Nothing -> putStrLn "No solution exists."
    Just d -> let sols = bruteForce d in do
      print d
      timeIO "then bruteforced in" $ mapM_ putStrLn $ map (asciiArtSolution d) sols
      assertMsgIO (show sols ++ "\n" ++ show d) $ length sols == 1

--TODO: prendre en param la stratégie, la tester sur tous les problèmes, et mesurer la meilleure.
--TODO: trouver des problèmes plus difficiles/grands, qui résistent à "(23*)*"

-- c'est à peu près toujours bien d'utiliser "3*" après "2"
-- "d" et "e" sont rarement utiles (mais peut-être plus rapides que 2 parfois ?).
-- "l" est strictement moins fort que 2.
-- avec (3*2)* on va très loin, mais parfois il faut les autres pour finir.

-- la partie droite de excellentHandMadePuz résiste bien à toutes les réductions actuelles. innover ?