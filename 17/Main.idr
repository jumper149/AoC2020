module Main

-- base
import System.File

main : IO ()
main = do
  rulesData <- readFile "./rules"
  messagesData <- readFile "./messages"
  pure ()
