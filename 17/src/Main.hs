{-# LANGUAGE RecordWildCards #-}

import Data.Functor
import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.String

data Activity = Active
              | Inactive
              deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Coordinate = Coordinate { xCoordinate :: Integer
                             , yCoordinate :: Integer
                             , zCoordinate :: Integer
                             }
              deriving (Eq, Ord, Read, Show)

type ActiveCubes = S.Set Coordinate

dataParser :: Parser ActiveCubes
dataParser = do
    bs <- many lineParser
    eof
    let cs = concat $ zipWith boolsToActiveCoords bs coords
    pure $ S.fromList cs
        where lineParser :: Parser [Bool]
              lineParser = many charParser <* newline
              charParser :: Parser Bool
              charParser = char '#' $> True <|> char '.' $> False
              boolsToActiveCoords :: [Bool] -> [Coordinate] -> [Coordinate]
              boolsToActiveCoords [] _ = []
              boolsToActiveCoords (True:bs) (c:cs) = c : boolsToActiveCoords bs cs
              boolsToActiveCoords (False:bs) (_:cs) = boolsToActiveCoords bs cs
              boolsToActiveCoords _ _ = undefined
              coords :: [[Coordinate]]
              coords = [ [ Coordinate x y 0 | x <- [ 0 .. ] ] | y <- [ 0 .. ] ]

main :: IO ()
main = do
    inputData <- parseFromFile dataParser "./data"
    case inputData of
      Left err -> print err
      Right dat -> print dat
    print inputData
    return ()
