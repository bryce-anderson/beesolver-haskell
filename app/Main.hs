module Main (main) where

import Lib

main :: IO ()
main = do
  dict <- makeDict Nothing
  putStrLn $ show dict
