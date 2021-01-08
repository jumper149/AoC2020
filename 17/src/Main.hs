{-# LANGUAGE RecordWildCards #-}

import Data.Functor
import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.String

data Coordinate = Coordinate { xCoordinate :: Integer
                             , yCoordinate :: Integer
                             , zCoordinate :: Integer
                             , wCoordinate :: Integer
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
              coords = [ [ Coordinate x y 0 0 | x <- [ 0 .. ] ] | y <- [ 0 .. ] ]

neighbors :: Coordinate -> [Coordinate]
neighbors c@(Coordinate x y z w) = do
    x' <- neighborsInteger x
    y' <- neighborsInteger y
    z' <- neighborsInteger z
    w' <- neighborsInteger w
    let c' = Coordinate x' y' z' w'
    filter (c /=) $ pure c'
        where neighborsInteger :: Integer -> [Integer]
              neighborsInteger i = [ pred i, i, succ i]

countActiveNeighbors :: ActiveCubes -> Coordinate -> Integer
countActiveNeighbors cubes cube =
    toEnum $ length $ filter isActive $ neighbors cube
        where isActive :: Coordinate -> Bool
              isActive c = c `S.member` cubes

activateActiveCube :: ActiveCubes -> Coordinate -> Bool
activateActiveCube cubes cube =
    case countActiveNeighbors cubes cube of
      2  -> True
      3  -> True
      _  -> False

activateInactiveCube :: ActiveCubes -> Coordinate -> Bool
activateInactiveCube cubes cube =
    case countActiveNeighbors cubes cube of
      3  -> True
      _  -> False

updateCubes :: ActiveCubes -> ActiveCubes
updateCubes actives = fromActive <> fromInactive
    where fromActive = S.filter (activateActiveCube actives) actives
          fromInactive = S.filter (activateInactiveCube actives) considerableInactives
          considerableInactives = S.fromList (concat $ neighbors <$> S.toList actives) S.\\ actives

times :: (a -> a) -> a -> Int -> a
times _ x 0 = x
times f x n
  | n < 0 = undefined
  | otherwise = f $ times f x $ pred n

main :: IO ()
main = do
    inputData <- parseFromFile dataParser "./data"
    case inputData of
      Left err -> print err
      Right cubes -> do
          let cubes' = times updateCubes cubes 6
          print $ S.size cubes'
    return ()
