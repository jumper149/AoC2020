{-# LANGUAGE RecordWildCards, TypeApplications #-}

data Password = Password {
    minCount :: Integer
  , maxCount :: Integer
  , char :: Char
  , pw :: String
  }
  deriving Show

parseLine :: String -> Password
parseLine str = Password {..}
  where rawData = words str
        minCount = read @ Integer $ rawData!!0
        maxCount = read @ Integer $ rawData!!1
        char = head $ rawData!!2
        pw = rawData!!3

isValidPW :: Password -> Bool
isValidPW Password {..} = count >= minCount && count <= maxCount
  where count = countElem char pw

countElem :: Eq a => a -> [a] -> Integer
countElem _ [] = 0
countElem x (y:ys) = if x == y
                        then succ $ countElem x ys
                        else countElem x ys

isValidPW' :: Password -> Bool
isValidPW' Password {..} = xor (char == fstOcc) (char == sndOcc)
  where fstOcc = pw !! fromInteger (pred minCount)
        sndOcc = pw !! fromInteger (pred maxCount)

xor :: Bool -> Bool -> Bool
xor False False = False
xor True False = True
xor False True = True
xor True True = False

main :: IO ()
main = do file <- readFile "./data_words"
          let inputData = parseLine <$> lines file
              solution1 = length $ filter id $ isValidPW <$> inputData
          print solution1
          let solution2 = length $ filter id $ isValidPW' <$> inputData
          print solution2
          return ()
