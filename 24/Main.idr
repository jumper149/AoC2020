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

Eq Direction where
  (==) East East = True
  (==) SouthEast SouthEast = True
  (==) SouthWest SouthWest = True
  (==) West West = True
  (==) NorthEast NorthEast = True
  (==) NorthWest NorthWest = True
  (==) _ _ = False

Show Direction where
  show East = "East"
  show SouthEast = "SouthEast"
  show SouthWest = "SouthWest"
  show West = "West"
  show NorthEast = "NorthEast"
  show NorthWest = "NorthWest"

namespace Grammar
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

  export
  dataGrammar : Grammar Char False (List (List Direction))
  dataGrammar = many directionsGrammar <* eof

record HexVector where
  constructor MkHexVector
  east, southeast, southwest : Integer

Eq HexVector where
  (==) (MkHexVector x y z) (MkHexVector x' y' z') = x == x' && y == y' && z == z'

Show HexVector where
  show (MkHexVector east southeast southwest) = "HexVector " ++ show east ++ " " ++ show southeast ++ " " ++ show southwest

namespace HexVector
  succ : Integer -> Integer
  succ = (+ 1)

  pred : Integer -> Integer
  pred = (+ 1)

  toHexVector : List Direction -> HexVector
  toHexVector [] = MkHexVector 0 0 0
  toHexVector (East :: xs) = record { east $= succ } (toHexVector xs)
  toHexVector (SouthEast :: xs) = record { southeast $= succ } (toHexVector xs)
  toHexVector (SouthWest :: xs) = record { southwest $= succ } (toHexVector xs)
  toHexVector (West :: xs) = record { east $= pred } (toHexVector xs)
  toHexVector (NorthEast :: xs) = record { southwest $= pred } (toHexVector xs)
  toHexVector (NorthWest :: xs) = record { southeast $= pred } (toHexVector xs)

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
              (Right (ds,cs)) => do
                print ds
                let vecs = toHexVector <$> ds
                print vecs
         pure ()
  pure ()
