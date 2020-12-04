{-# LANGUAGE RecordWildCards, TypeApplications #-}

parseChar :: Char -> Bool
parseChar '#' = True
parseChar '.' = False
parseChar _ = undefined

countTrees :: Int -> [[Bool]] -> Integer
countTrees i [] = 0
countTrees i (firstTrees:lastTrees) =
  if tree
     then succ $ rest
     else rest
  where tree = firstTrees !! (3 * i)
        rest = countTrees (succ i) lastTrees

countTrees' :: Int -> Int -> [[Bool]] -> Integer
countTrees' mult i [] = 0
countTrees' mult i (firstTrees:lastTrees) =
  if tree
     then succ $ rest
     else rest
  where tree = firstTrees !! (mult * i)
        rest = countTrees' mult (succ i) lastTrees

countTrees'' :: Int -> [[Bool]] -> Integer
countTrees'' i [] = 0
countTrees'' i (firstTrees:_:lastTrees) =
  if tree
     then succ $ rest
     else rest
  where tree = firstTrees !! (1 * i)
        rest = countTrees'' (succ i) lastTrees
countTrees'' i (firstTrees:lastTrees) = 0

main :: IO ()
main = do file <- readFile "./data"
          let inputData = map parseChar <$> lines file
              treeHill = cycle <$> inputData
              solution = countTrees 0 treeHill
          print solution
          let solution2 = product [ countTrees' 1 0 treeHill
                                  , countTrees' 3 0 treeHill
                                  , countTrees' 5 0 treeHill
                                  , countTrees' 7 0 treeHill
                                  , countTrees'' 0 treeHill
                                  ]
          print solution2
          return ()
