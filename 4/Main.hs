{-# LANGUAGE RecordWildCards, TypeApplications #-}

import Data.List (sort)
import Data.Maybe (isJust)
import Text.Read (readMaybe)

splitParagraphs :: [String] -> [String] -> [[String]]
splitParagraphs [] acc = [acc]
splitParagraphs ("":xs) acc = acc : splitParagraphs xs []
splitParagraphs (x:xs) acc = splitParagraphs xs (x:acc)

removeCID :: [String] -> [String]
removeCID [] = []
removeCID (x:xs) = case x of
  'c':'i':'d':_ -> removeCID xs
  y -> y : removeCID xs

isValid :: [String] -> Bool
isValid ["byr","ecl","eyr","hcl","hgt","iyr","pid"] = True
isValid _ = False

data ID = ID { byr :: Integer
             , iyr :: Integer
             , eyr :: Integer
             , hgt :: Height
             , hcl :: String
             , ecl :: String
             , pid :: String
             }
  deriving Show

data Height = Cm Integer
            | In Integer
  deriving Show

validateHeight :: Height -> Bool
validateHeight (Cm x) = 150 <= x && x <= 193
validateHeight (In x) = 59 <= x && x <= 76

validateEyeColor :: String -> Bool
validateEyeColor ('0':xs) = validateEyeColor xs
validateEyeColor ('1':xs) = validateEyeColor xs
validateEyeColor ('2':xs) = validateEyeColor xs
validateEyeColor ('3':xs) = validateEyeColor xs
validateEyeColor ('4':xs) = validateEyeColor xs
validateEyeColor ('5':xs) = validateEyeColor xs
validateEyeColor ('6':xs) = validateEyeColor xs
validateEyeColor ('7':xs) = validateEyeColor xs
validateEyeColor ('8':xs) = validateEyeColor xs
validateEyeColor ('9':xs) = validateEyeColor xs
validateEyeColor ('a':xs) = validateEyeColor xs
validateEyeColor ('b':xs) = validateEyeColor xs
validateEyeColor ('c':xs) = validateEyeColor xs
validateEyeColor ('d':xs) = validateEyeColor xs
validateEyeColor ('e':xs) = validateEyeColor xs
validateEyeColor ('f':xs) = validateEyeColor xs
validateEyeColor (_:xs) = False
validateEyeColor [] = True

validatePID :: String -> Bool
validatePID ('0':xs) = validatePID xs
validatePID ('1':xs) = validatePID xs
validatePID ('2':xs) = validatePID xs
validatePID ('3':xs) = validatePID xs
validatePID ('4':xs) = validatePID xs
validatePID ('5':xs) = validatePID xs
validatePID ('6':xs) = validatePID xs
validatePID ('7':xs) = validatePID xs
validatePID ('8':xs) = validatePID xs
validatePID ('9':xs) = validatePID xs
validatePID (_:xs) = False
validatePID [] = True

parseID :: [String] -> Maybe ID
parseID [byrStr,eclStr,eyrStr,hclStr,hgtStr,iyrStr,pidStr] = do
  byr' <- readMaybe $ drop 4 byrStr
  byr <- if byr' >= 1920 && byr' <= 2002
            then Just byr'
            else Nothing
  iyr' <- readMaybe $ drop 4 iyrStr
  iyr <- if iyr' >= 2010 && iyr' <= 2020
            then Just iyr'
            else Nothing
  eyr' <- readMaybe $ drop 4 eyrStr
  eyr <- if eyr' >= 2020 && eyr' <= 2030
            then Just iyr'
            else Nothing
  let hgt' = drop 4 hgtStr
  hgt'' <- case reverse hgt' of
             'm':'c':l -> fmap Cm $ readMaybe $ reverse l
             'n':'i':l -> fmap In $ readMaybe $ reverse l
             _ -> Nothing
  hgt <- if validateHeight hgt'' then Just hgt'' else Nothing
  let ecl' = drop 4 eclStr
  ecl <- case ecl' of
           "amb" -> Just ecl'
           "blu" -> Just ecl'
           "brn" -> Just ecl'
           "gry" -> Just ecl'
           "grn" -> Just ecl'
           "hzl" -> Just ecl'
           "oth" -> Just ecl'
           _ -> Nothing
  let hcl'' = drop 4 hclStr
  hcl' <- if length hcl'' > 0
             then if head hcl'' == '#'
                  then Just $ tail hcl''
                  else Nothing
             else Nothing
  hcl <- if length hcl' == 6
            then if validateEyeColor hcl'
                 then Just hcl'
                 else Nothing
            else Nothing
  let pid' = drop 4 pidStr
  pid <- if length pid' == 9
            then if validatePID pid'
                 then Just pid'
                 else Nothing
            else Nothing
  return ID {..}
parseID _ = Nothing

isValid' :: [String] -> Bool
isValid' = isValid . map (take 3)

main :: IO ()
main = do file <- readFile "./data_lines"
          let inputData = map sort $ splitParagraphs (removeCID $ lines file) []
              solution1 = length $ filter isValid inputData
          print solution1
          file2 <- readFile "./data_lines2"
          let inputData2 = map sort $ splitParagraphs (removeCID $ lines file2) []
              solution2 = length $ filter isJust $ map parseID inputData2
          print solution2
          return ()
