{-# LANGUAGE RecordWildCards, TypeApplications #-}

import qualified Control.Monad.State as St
import Data.Foldable (fold)
import Data.List (sort)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec


data Color = Color
  { style :: String
  , chromaticity :: String
  }
  deriving (Eq, Ord, Read, Show)

data Bag = Bag
  { color :: Color
  , containedBags :: [(Color,Integer)]
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

parseContainedBags :: Parsec String () [(Color,Integer)]
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

containmentsOf :: [Bag] -> Color -> [Color]
containmentsOf bs c = color <$> filter contains bs
  where contains b = c `elem` (fst <$> containedBags b)

recursiveContainments :: [Bag] -> Color -> St.State (S.Set Color) (S.Set Color)
recursiveContainments bs c = do acc <- St.get
                                let newAcc = acc <> currentContainments
                                if acc == newAcc
                                   then return acc
                                   else do
                                     St.put newAcc
                                     cs <- traverse (recursiveContainments bs) $ S.toList (newAcc S.\\ acc)
                                     return $ fold cs
  where currentContainments = S.fromList $ containmentsOf bs c

countBags :: M.Map Color Bag -> Color -> Integer
countBags m c = 1 + case insideBags of
  Nothing -> undefined
  Just bs -> sum $ multInsideBag <$> bs
  where insideBags = containedBags <$> M.lookup c m
        multInsideBag (inC,inI) = inI * countBags m inC

toBagMap :: [Bag] -> M.Map Color Bag
toBagMap bs = M.fromList $ (\ b -> (color b,b)) <$> bs

main :: IO ()
main = do file <- readFile "./data"
          let bagData = parse parseBagData "data" file
              colorOfChoice = Color
                { style = "shiny"
                , chromaticity = "gold"
                }
          case bagData of
            Left err -> print err
            Right actualBagData -> do
              print $ length $ S.toList $ St.evalState (recursiveContainments actualBagData colorOfChoice) mempty
              print $ countBags (toBagMap actualBagData) colorOfChoice - 1
          return ()
