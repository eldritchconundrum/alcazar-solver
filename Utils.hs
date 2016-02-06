module Utils (
  empty, first, contains, count, time, timeIO
  ) where
import Data.Time
import Control.Exception.Base (evaluate)
import System.IO

empty = null -- null is a pointer thing, show me the high level view
first = head -- The opposite of 'last' is not called 'head'. At least not in my head. That would be a first.
xs `contains` x = x `elem` xs -- come on, 'elem' is not even a word. you can do better than that, stdlib
count f = length . filter f

timeIO :: String -> IO a -> IO a
timeIO message action = do
  t0 <- getCurrentTime
  result <- action
  t1 <- getCurrentTime
  putStrLn $ message ++ " " ++ show (diffUTCTime t1 t0)
  return result

time :: String -> a -> IO a
time message value = timeIO message (evaluate value)

{-
strictEvalIO is not needed anymore since I found Control.Exception.Base.evaluate.

strictEvalIO :: Show a => a -> IO a -- This assumes show deeply evaluates.
strictEvalIO x = do
  writeFile "/dev/null" (show x)
  return x

-- pour tester : solveAndPrint "l*d3*b*3*" valentinePuz

strictEvalIO_ThatDoesntWork :: Show a => a -> IO a -- This assumes show deeply evaluates.
strictEvalIO_ThatDoesntWork x = do
  let y = (show . length . show) x
  return (seq y x)
-}
