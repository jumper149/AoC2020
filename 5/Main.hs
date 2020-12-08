{-# LANGUAGE RecordWildCards, TypeApplications #-}

toBoolLists :: String -> ([Bool],[Bool])
toBoolLists str = (rowToBoolList $ drop 3 str , lrToBoolList $ take 3 str)

rowToBoolList :: String -> [Bool]
rowToBoolList ('F':xs) = False : rowToBoolList xs
rowToBoolList ('B':xs) = True : rowToBoolList xs
rowToBoolList [] = []

lrToBoolList :: String -> [Bool]
lrToBoolList ('L':xs) = False : lrToBoolList xs
lrToBoolList ('R':xs) = True : lrToBoolList xs
lrToBoolList [] = []

-- Start with exponent 0.
parseSeat :: [Bool]
          -> Int -- ^ exponent
          -> Int
parseSeat [] _ = 0
parseSeat (x:xs) exponent = digitFactor * currentDigit + rest
  where currentDigit = 2^exponent
        rest = parseSeat xs (succ exponent)
        digitFactor = case x of
                        False -> 0
                        True -> 1

parseSeatID :: ([Bool],[Bool]) -> Int
parseSeatID (rows,lrs) = parseSeat rows 0 * 8 + parseSeat lrs 0

main :: IO ()
main = do file <- readFile "./data"
          let inputData = map reverse $ lines file
              solution1 = parseSeatID . toBoolLists <$> inputData
          print $ maximum solution1
