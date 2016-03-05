module Strategy (
  Strategy(..), ReduceFunction, parseStrategy
  ) where

type ReduceFunction = Char

data Strategy = SingleReduction ReduceFunction
              | Reductions [Strategy] -- ()
              | Repetition Strategy -- *
              deriving Eq

instance Show Strategy where
  show (SingleReduction rf) = [rf]
  show (Reductions sts) = show sts
  show (Repetition st) = show st ++ "*"

instance Read Strategy where
  --readsPrec :: Int -> String -> [(Strategy, String)]
  readsPrec prec s = [parse [] s]

parse :: [Strategy] -> String -> (Strategy, String) -- [Strategy] is a reversed buffer of already read strategies
parse acc "" = (Reductions (reverse acc), "")
parse acc (')':cs) = (Reductions (reverse acc), cs)
parse acc ('(':cs) = parse strats rest where strats = inner : acc
                                             (inner, rest) = parse [] cs
parse acc ('*':cs) = parse strats cs where strats = Repetition (head acc) : tail acc
parse acc (c:cs) = parse strats cs where strats = SingleReduction c : acc


parseStrategy = canonicalizeStrategy . read

canonicalizeStrategy (Reductions strats) =
  case [s |
        s <- map canonicalizeStrategy strats,
        s /= (Reductions [])]
  of [s] -> s
     ss -> Reductions ss
canonicalizeStrategy (Repetition strat) = Repetition (canonicalizeStrategy strat)
canonicalizeStrategy strat = strat

