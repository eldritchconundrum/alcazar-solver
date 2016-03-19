import Data.List
import Utils
import Puzzle
import Graph
import Solver
import TestData

-- rm -f *.hi && ghci Main.hs
-- rm -f *.hi *.o && ghc -O Main.hs && time ./Main

sameReductions :: (String, String) -> Puzzle -> Bool -- TODO: utiliser NotBetter plutôt 
sameReductions (sg1, sg2) puz = let
  d1 = (extract . reduceStrategy sg1 . start) puz
  d2 = (extract . reduceStrategy sg2 . start) puz
  in paths d1 == paths d2 && doorStatus d1 == doorStatus d2 && adjacency (graph d1) == adjacency (graph d2)

assertMsgIO s b = if b then return () else error s
assertMsg s b = if b then id else error s

tests :: IO ()
tests = do
  assertMsgIO "no sols" $ 0 == (length . bruteForce . buildInitialData) noSolutionPuz
  assertMsgIO "trivial" $ 1 == (length . bruteForce . buildInitialData) trivialPuz
  assertMsgIO "multi  " $ 2 == (length . bruteForce . buildInitialData) multipleSolutionPuz
  assertMsgIO "deduceDoorStatus" $ not $ sameReductions ("3*", "3*d") fivePuz
  assertMsgIO "lackOfNeighbors" $ not $ sameReductions ("3*", "3*l") provablyUselessDoorPuz
  assertMsgIO "bouncing" $ not $ sameReductions ("3*", "3*b") jsPuz
  assertMsgIO "bouncing2" $ sameReductions ("3*bb", "3*b*") jsPuz
  assertMsgIO "big" $ 1 == (length . bruteForce . extract . reduceStrategy "3*23*" . start) unitTestReduce3RemoveEdges

m = main
main = do
  --timeAll defaultStrategy
  tests
  --mapM_ (solveAndPrint "3*") instantaneousPuzzles
  --printReduced "3*eb*3*2*" feb13
  --printReduced "3*d3*23*23*23*23*" valentinePuz --solveAndPrint "3*d3*" valentinePuz
  --printReduced "3*d3*b*e3*b*23*23*23*" daily6x9Puz -- solveAndPrint "3*d3*b*e3*b*" daily6x9Puz -- js: 23m
  --solveAndPrint "3*b*3*b22223*23*b" daily7x10Puz  --printReduced "3*b*3*b" daily7x10Puz
  --printReduced "3*e*3*2323*23*23*2223*23*23*23*23*23*23*23*" dailyOther7x10 --printReduced "3*e*3*" dailyOther7x10
  --printReduced "3*b*3*22222222223*23*d3*dl" manyDoors -- printReduced "3*b*3*" manyDoors -- js: même pas en rêve
  --printReduced "3*d3*23*223*23*23*23*222222223*23*23*" book10x15Puz -- printReduced "3*d3*" book10x15Puz -- js: même pas en rêve
  --printReduced "3*b*3*2*" mars4Puz
  --solveAndPrint "3*b*2*" sixRoomsPuz -- js: >46h
  --solveAndPrint "3*b*223*2223*23*23*" jsPuz -- js: 0s
  --solveAndPrint "3*2*" excellentHandMadePuz -- js: 21h
  --solveAndPrint defaultStrategy mar17
  printReduced defaultStrategy alcazam

printReduced strat puz = timeIO "reduced in" $ (print . reduceStrategy strat . start) puz
solveAndPrint strat puz = do
  reduced <- time "reduced in" $ (toMaybe . reduceStrategy strat . start) puz
  case reduced of
    Nothing -> putStrLn "No solution exists."
    Just d -> let sols = bruteForce d in do
      print d
      -- TODO: print the solution only once if bruteforce is not needed after the reduction
      timeIO "then bruteforced in" $ mapM_ putStrLn $ map (asciiArtSolution d) sols
      assertMsgIO (show sols ++ "\n" ++ show d) $ length sols == 1


timeAll strat = timeIO "reduce all" $ sequence_ $ map (printReduced strat) allPuzzles

--defaultStrategy = "(((((3*b)*e)*l)*2)*d)*" -- 1.47, 25.9  -- lent sur unitTestReduce3RemoveEdges car "3*23*" suffirait
--defaultStrategy = "(((3*b)*2el)*d)*" -- 1.57, 25.8
--defaultStrategy = "((3*bel2)*d)*" -- 1.59, 15.8
--defaultStrategy = "(((3*bel)*2)*d)*" -- 1.46, 19.8
--defaultStrategy = "((((3*2)*el)*d)*b)*" -- 1.44, 8.0
defaultStrategy = "(((3*2)*eld)*b)*" -- 1.44, 8.0

-- "((((3*2)*eld)*b)*X²)*"


makeMoreDifficult puz = mapM_ try (moreDifficult puz) where -- by keeping it reducable by defaultStrategy
  try :: Puzzle -> IO ()
  try p = do
    printReduced defaultStrategy p
    putStrLn $ showPuzzle p
  moreDifficult :: Puzzle -> [Puzzle] -- by removing walls
  moreDifficult puz =
    let listsWithoutOneElt list = [delete x list | x <- list]
        (sz, ds, wd, wr) = (size puz, doors puz, wallsDown puz, wallsRight puz)
    in [buildPuzzle sz ds wd' wr | wd' <- listsWithoutOneElt wd] ++
       [buildPuzzle sz ds wd wr' | wr' <- listsWithoutOneElt wr]



{-

Réductions et résolutions :

Pour l'instant j'ai un bruteforce, mais avant de l'exécuter je
simplifie le problème en déduisant des faits de la même manière que je
résoudrais manuellement. Les techniques de résolution ont une lettre
associée.

3 : localisation des paths de longueur 3 (réduction des noeuds sans
porte avec seulement deux voisins). Immédiat.

b : localisation des paths de longueur 2 (aka "bouncing" : quand en enlevant un edge, le
problème devient prouvablement impossible car un noeud n'aurait plus comme voisins que les endpoints d'un même path).

automatique en début :
exploitation de la parité : sur quels (x+y)`mod`2 sont les doors ?
(une fois, au début, dans puzzle, pas dans graph)

e : les portes qui ont déjà un path n'ont pas besoin d'autres edges.
d : un noeud avec plus qu'un voisin doit être une porte.
l : si sans un edge, un noeud n'a plus assez de voisin pour que ce soit finissable, alors c'est un path.
2 : pareil que 'l' mais on réduit par 3* avant de tester.

"l" est strictement moins fort que 2.
"d" et "e" sont rarement utiles (mais peut-être plus rapides que 2 parfois ?).


TODO: réduction "réduction sur toutes les doorPairs, élimination de ceux qui conduisent à des impossibilités, et récupération de l'intersection des paths/reducs trouvés partout"

TODO: composantes fortement connexes. Si > 1, pas de solution.
--> si deux sous-graphes ne sont connectés que par un seul edge, alors il y a une door oblig de chaque côté (test : multiple4x4RoomsWithDoors)
trouver s'il existe un moyen plus efficace que le moyen naïf de savoir
quels edges d'un graphe conduiraient à > 1 composantes connexes s'ils
étaient enlevés ?
  hmm c'est quoi le moyen naïf déjà ? O(n^?) ?
  trouver d'abord les cycles, puis essayer les edges pas entre deux noeuds d'un cycle ?

TODO: tenter de découper le graphe en deux "zones" de superficie pas trop inégale qui ne communiquent que via 1 ou 2 bottlenecks, résoudre/simplifier les sous-graphes, puis combiner ça


TODO: Voir quels puzzles ne sont pas résolus sans bruteForce ou réduits trop lentement.
Trouver des problèmes plus difficiles/grands, qui résistent à defaultStrategy. combiner des existants ?

l'idée de stratégie en tant que DSL n'est pas une utilisation extraordinairement adaptée d'un DSL, vu qu'on est presque seulement intéressé par ce qui est de la forme (((A*B)*C)*. Mais ça offre aussi la possibilité d'afficher les réductions qui ont été réalisées sous forme visualisable simplement. TODO: le faire

A TESTER : que deviens le code si on représente les doors potentielles comme étant connectées dans le graphe à un même fake node invisible ? plus simple ? ça unifie probablement certaines réductions.

backtracking : l'intégrer au reste du moteur quand on ne sait plus réduire, au lieu d'être une alternative séparée comme actuellement.

TODO: un éditeur de niveaux pour écrire des tests plus facilement. dans mes rêves.

TODO: tester Control.Concurrent.compete pour le fun

TO NOT DO: profiling, changer les structures de données en Array ou
autre, voir si c'est plus performant... ne pas faire ça parce que 1)
rester high-level c'est plus rigolo, 2) ça marche pas, je ne sais pas
faire, à chaque fois que j'essaye d'optimiser du haskell c'est juste
plus lent après (et mon code est moins lisible).

-}

