module Main (main) where

import Lib

data Config = Config {
   dictPath :: Maybe String,
   required :: Char,
   others :: String
  } deriving (Show)

getConfig :: IO Config
getConfig = do
  return Config { dictPath = Nothing, required = 'a', others = "abcdefg" }

main :: IO ()
main = do
  config <- getConfig
  helloMessage
  dict <- makeDict (dictPath config)
  putStrLn $ show dict

helloMessage :: IO ()
helloMessage = do
  putStrLn "🐝"
  putStrLn "Hello and welcome to Spelling Bee Solver"
  putStrLn "🐝🐝"

  putStrLn "🐝🐝🐝"
  -- putStrLn "Required Letter:  ", string(required)
  -- putStrLn "Other Letters:    ", others
  -- putStrLn "Dictionary:       ", dictionaryName
  -- putStrLn "Dictionary words: ", len(dictionary.Words())
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

