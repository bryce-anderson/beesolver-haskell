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
  putStrLn "🐝"
  putStrLn "Hello and welcome to Spelling Bee Solver"
  putStrLn "🐝🐝"
  putStrLn "🐝🐝🐝"
  putStrLn $ "Required Letter:  " ++ (required config)
  putStrLn $ "Other Letters:    " ++ (others config)
  putStrLn $ "Dictionary:       " ++ (dict config)
  putStrLn $ "Dictionary words: " ++ (show (dictSize d))
  putStrLn "Solving now"
  let puzzle = makePuzzle (head $ required config) (others config)
  let results = solvePuzzle d puzzle
  putStrLn "🐝🐝🐝🐝"
  putStrLn "🐝🐝🐝🐝🐝"
  putStrLn "Solved!"
  putStrLn ""
  putStrLn $ "Matching words " ++ (show $ length results)
  if wordsoutput config then print $ map translate results
  else return ()
    where
      translate (Valid word True) = word ++ " 🍳"
      translate (Valid word False) = word
      translate (Invalid _ ) = error "shouldn't get here"

