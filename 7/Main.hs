{-# LANGUAGE RecordWildCards, TypeApplications #-}

import Data.List (sort)
import Data.Set
import Text.Parsec

data Color = Color
  { style :: String
  , chromaticity :: String
  }
  deriving (Eq, Ord, Read, Show)

data Bag = Bag
  { color :: Color
  , containedBags :: [(Color,Int)]
  }
  deriving (Eq, Ord, Read, Show)

parseBag :: Parsec String () Bag
parseBag = do
  style <- many lower
  space
  chromaticity <- many lower
  space
  string "bags"
  space
  string "contain"
  containedBags <- try parseNoOtherBags <|> parseContainedBags
  let color = Color {..}
  return Bag {..}
  where parseNoOtherBags = do
          space
          string "no other bags."
          return []

parseContainedBags :: Parsec String () [(Color,Int)]
parseContainedBags = do
  space
  amount <- many1 digit
  space
  style <- many lower
  space
  chromaticity <- many lower
  space
  string "bag"
  optional $ char 's'
  let currentBag = (Color {..}, read amount)
      wrapUpLastBag = do
        char '.'
        return [currentBag]
      parseMoreBags = do
        char ','
        (currentBag :) <$> parseContainedBags
  wrapUpLastBag <|> parseMoreBags

parseBagData :: Parsec String () [Bag]
parseBagData = do
  bags <- manyTill parseSingleBag eof
  return bags
  where parseSingleBag = do
          bag <- parseBag
          newline
          return bag

containmentsOf :: Color -> [Bag] -> [Color]
containmentsOf c bs = color <$> filter contains bs
  where contains b = c `elem` (fst <$> containedBags b)

recursiveContainments :: [Bag] -> Color -> State (Set Color) (Set Color)
recursiveContainments bs c = do acc <- get
                                let newAcc = acc <> currentContainments
  where currentContainments = containmentsOf c bs

main :: IO ()
main = do file <- readFile "./data"
          let bagData = parse parseBagData "data" file
              colorOfChoice = Color
                { style = "shiny"
                , chromaticity = "gold"
                }
          print $ length <$> bagData
          print . fmap length $ containmentsOf colorOfChoice <$> bagData
          return ()
