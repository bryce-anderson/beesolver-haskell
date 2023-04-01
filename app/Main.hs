{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import Lib
import System.Console.CmdArgs


data Config = Config
  { dict :: String
  , wordsoutput :: Bool
  , required :: String
  , others :: String
  } deriving (Show, Data, Typeable)


getConfig :: IO Config
getConfig = checkValid <$> cmdArgsRun mode
  where 
    mode = cmdArgsMode config
    config = Config
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

main :: IO ()
main = do
  config <- getConfig
  d <- makeDict (dict config)
  helloMessage config d
  let puzzle = makePuzzle (head $ required config) (others config)
  let results = solvePuzzle d puzzle
  print puzzle
  showResults config results

showResults :: Config -> [Result] -> IO ()
showResults cfg results = do
  putStrLn $ "Matching words " ++ (show $ length results) 
  if wordsoutput cfg then print $ map translate results
  else return ()
    where
      translate (Valid word True) = word ++ " 🍳"
      translate (Valid word False) = word
      translate (Invalid _ ) = error "shouldn't get here"

helloMessage :: Config -> Dictionary -> IO ()
helloMessage config dictionary = do
  putStrLn "🐝"
  putStrLn "Hello and welcome to Spelling Bee Solver"
  putStrLn "🐝🐝"

  putStrLn "🐝🐝🐝"
  putStrLn $ "Required Letter:  " ++ (required config)
  putStrLn $ "Other Letters:    " ++ (others config)
  putStrLn $ "Dictionary:       " ++ (dict config)
  putStrLn $ "Dictionary words: " ++ (show (dictSize dictionary))
  putStrLn "Solving now"
  putStrLn "🐝🐝🐝🐝"

  -- solver := Solver{dictionary, puzzle}
  -- start = time.Now()
  -- solutions := solver.Solve()
  -- elapsedSolve := time.Since(start)

  putStrLn "🐝🐝🐝🐝🐝"
  putStrLn "Solved!"
  putStrLn ""
  -- putStrLn "  Words:", len(solutions)

