module Main

-- base
import System.File

-- contrib
import Text.Parser
import Text.Parser.Core

data Direction = East
               | SouthEast
               | SouthWest
               | West
               | NorthEast
               | NorthWest

Show Direction where
  show East = "East"
  show SouthEast = "SouthEast"
  show SouthWest = "SouthWest"
  show West = "West"
  show NorthEast = "NorthEast"
  show NorthWest = "NorthWest"

eastWestGrammar : Grammar Char True Direction
eastWestGrammar = terminal "Can't parse East" $ \ x =>
                  case x of
                       'e' => Just East
                       'w' => Just West
                       _ => Nothing

northGrammar : Grammar Char True Direction
northGrammar = terminal "Can't parse North" checkN *> terminal "Can't parse North-?" checkEW where
  checkN : Char -> Maybe ()
  checkN 'n' = Just ()
  checkN _ = Nothing

  checkEW : Char -> Maybe Direction
  checkEW 'e' = Just NorthEast
  checkEW 'w' = Just NorthWest
  checkEW _ = Nothing

southGrammar : Grammar Char True Direction
southGrammar = terminal "Can't parse South" checkS *> terminal "Can't parse South-?" checkEW where
  checkS : Char -> Maybe ()
  checkS 's' = Just ()
  checkS _ = Nothing

  checkEW : Char -> Maybe Direction
  checkEW 'e' = Just SouthEast
  checkEW 'w' = Just SouthWest
  checkEW _ = Nothing

directionGrammar : Grammar Char True Direction
directionGrammar = eastWestGrammar <|> northGrammar <|> southGrammar

directionsGrammar : Grammar Char True (List Direction)
directionsGrammar = many directionGrammar <* terminal "Can't parse newline" checkNewline where
  checkNewline : Char -> Maybe ()
  checkNewline '\n' = Just ()
  checkNewline _ = Nothing

dataGrammar : Grammar Char False (List (List Direction))
dataGrammar = many directionsGrammar <* eof

main : IO ()
main = do
  inputData <- readFile "./data"
  case inputData of
       Left err => print err
       Right dat => do
         let tokenList = unpack dat
             directions = parse dataGrammar tokenList
         case directions of
              (Left (Error err _)) => putStrLn err
              (Right (ds,cs)) => print ds
         pure ()
  pure ()
