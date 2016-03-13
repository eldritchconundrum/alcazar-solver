module Strategy (
  Strategy(..), ReduceFunction, parseStrategy
  ) where

type ReduceFunction = Char

data Strategy = SingleReduction ReduceFunction
              | Reductions [Strategy] -- ()
              | Repetition Strategy -- *
              deriving Eq

parseStrategy :: String -> Strategy
parseStrategy = read

instance Show Strategy where
  show (SingleReduction rf) = [rf]
  show (Reductions sts) = show sts
  show (Repetition st) = show st ++ "*"

instance Read Strategy where
  --readsPrec :: Int -> String -> [(Strategy, String)]
  readsPrec prec s = [parse [] s]

parse :: [Strategy] -> String -> (Strategy, String)
parse acc "" = (strategyFromList (reverse acc), "")
parse acc (')':cs) = (strategyFromList (reverse acc), cs)
parse acc ('(':cs) = parse strats rest where strats = inner : acc
                                             (inner, rest) = parse [] cs
parse acc ('*':cs) = parse strats cs where strats = Repetition (head acc) : tail acc
parse acc (c:cs) = parse strats cs where strats = SingleReduction c : acc

strategyFromList :: [Strategy] -> Strategy
strategyFromList [s] = s -- simplify a little
strategyFromList ss = Reductions ss
