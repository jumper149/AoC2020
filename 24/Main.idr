module Main

-- base
import Data.List
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
  pred x = x - 1

  export
  toHexVector : List Direction -> HexVector
  toHexVector [] = MkHexVector 0 0 0
  toHexVector (East :: xs) = record { east $= succ } (toHexVector xs)
  toHexVector (SouthEast :: xs) = record { southeast $= succ } (toHexVector xs)
  toHexVector (SouthWest :: xs) = record { southwest $= succ } (toHexVector xs)
  toHexVector (West :: xs) = record { east $= pred } (toHexVector xs)
  toHexVector (NorthEast :: xs) = record { southwest $= pred } (toHexVector xs)
  toHexVector (NorthWest :: xs) = record { southeast $= pred } (toHexVector xs)

record Coordinate where
  constructor MkCoordinate
  xCoord, yCoord : Integer

Eq Coordinate where
  (==) (MkCoordinate x y) (MkCoordinate x' y') = x == x' && y == y'

Show Coordinate where
  show (MkCoordinate x y) = "Coordinate " ++ show x ++ " " ++ show y

toCoordinate : HexVector -> Coordinate
toCoordinate (MkHexVector east southeast southwest) = MkCoordinate x y where
  x : Integer
  x = 2 * east + southeast - southwest
  y : Integer
  y = southeast + southwest

namespace countElements
  elements : Eq a => List a -> List a -> List a
  elements [] acc = acc
  elements (x :: xs) acc with (find (== x) acc)
    elements (x :: xs) acc | Nothing = elements xs (x :: acc)
    elements (x :: xs) acc | (Just _) = elements xs acc

  occurrences : Eq a => a -> List a -> Nat
  occurrences x [] = Z
  occurrences x (y :: xs) with (x == y)
    occurrences x (y :: xs) | True = S $ occurrences x xs
    occurrences x (y :: xs) | False = occurrences x xs

  export
  countElements : List Coordinate -> List (Coordinate,Nat)
  countElements xs = f <$> elements xs [] where
    f : Coordinate -> (Coordinate,Nat)
    f x = (x,occurrences x xs)

mutual
  even : Nat -> Bool
  even Z = True
  even (S k) = odd k

  odd : Nat -> Bool
  odd Z = False
  odd (S k) = even k

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
                --print ds
                let vecs = toHexVector <$> ds
                --print vecs
                let coords = toCoordinate <$> vecs
                --print coords
                let counts = countElements coords
                    flips = snd <$> counts
                    countBlacks = length $ filter odd flips
                print countBlacks
         pure ()
  pure ()
