module Main

main : IO ()
main = do
  inputData <- readFile "./data"
  print inputData
