module Main

import System.File

main : IO ()
main = do
  inputData <- readFile "./data"
  print inputData
  pure ()
