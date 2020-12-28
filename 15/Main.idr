module Main

-- base
import Data.List
import System.File

-- contrib
import Data.String.Parser

main : IO ()
main = do
  inputData <- readFile "./data"
  case inputData of
       Left err => print err
       Right dat => do
         let startingNumbers' = parse inputParser
         case startingNumbers' of
              (Left err) => putStrLn err
              (Right startingNumbers) => do
                print startingNumbers
         pure ()
  pure ()
