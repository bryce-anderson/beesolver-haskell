{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import Lib
import System.Clock (TimeSpec(TimeSpec), getTime, Clock(Monotonic), diffTimeSpec)
import System.Console.CmdArgs


data BeeSolver = BeeSolver 
  { dict :: String
  , wordsoutput :: Bool
  , required :: String
  , others :: String
  } deriving (Show, Data, Typeable)


getBeeSolver :: IO BeeSolver
getBeeSolver = checkValid <$> cmdArgsRun mode
  where 
    mode = cmdArgsMode beeSolver
    beeSolver = BeeSolver
      { dict        = "american-english-large" &= help "Path to custom dictionary"
      , wordsoutput = True &= help "Output matching words"
      , required    = def &= typ "REQUIRED" &= argPos 0
      , others      = def &= typ "OTHERS"   &= argPos 1
      } &= help "Solve a Puzzle"
    checkValid m = 
      if invalid then error "args are invalid"
      else m
        where
        invalid = length (required m) /= 1 || length (others m) /= 6

timeNow :: IO TimeSpec
timeNow = getTime Monotonic

main :: IO ()
main = do
  beeSolver <- getBeeSolver
  dictLoadStart <- timeNow
  d <- makeDict (dict beeSolver)
  putStrLn "🐝"
  putStrLn "Hello and welcome to Spelling Bee Solver"
  putStrLn "🐝🐝"
  putStrLn "🐝🐝🐝"
  putStrLn $ "Required Letter:  " ++ (required beeSolver)
  putStrLn $ "Other Letters:    " ++ (others beeSolver)
  putStrLn $ "Dictionary:       " ++ (dict beeSolver)
  putStrLn $ "Dictionary words: " ++ (show (dictSize d))
  let puzzle = makePuzzle (head $ required beeSolver) (others beeSolver)
  putStrLn "Solving now"
  startSolve <- getTime Monotonic
  let results = solvePuzzle d puzzle
  putStrLn "🐝🐝🐝🐝"
  putStrLn "🐝🐝🐝🐝🐝"
  putStrLn "Solved!"
  putStrLn ""
  dumpOutput beeSolver results
  dumpTimings dictLoadStart startSolve

dumpTimings :: TimeSpec -> TimeSpec -> IO ()
dumpTimings dictLoadStart startSolve = do
  solveEnd <- timeNow
  putStrLn $ "  Time loading dictionary: " ++ toMsString (startSolve `diffTimeSpec` dictLoadStart)
  putStrLn $ "  Time solving: " ++ toMsString (solveEnd `diffTimeSpec` startSolve)
  where
    toMsString timespec = show (toMs timespec) ++ " ms"
    toMs (TimeSpec sec nanos) = sec * 1000 + (nanos `div` 1000000)

dumpOutput :: BeeSolver -> [Result] -> IO ()
dumpOutput beeSolver results = do
  putStrLn $ "Matching words " ++ (show $ length results)
  putStrLn $ "Pangrams: " ++ (show $ length $ filter isPangram results)
  if wordsoutput beeSolver then print $ map translate results
  else return ()
    where
      translate (Valid word True) = word ++ " 🍳"
      translate (Valid word False) = word
      translate (Invalid _ ) = error "shouldn't get here"

