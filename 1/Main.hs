{-# LANGUAGE TypeApplications #-}

main :: IO ()
main = do file <- readFile "./data"
          let inputData = read @ Integer <$> lines file
              solution1 = filter ((/=) 0) $ [ if x + y == 2020 then x * y else 0 | x <- inputData, y <- inputData ]
          print solution1
          let solution2 = filter ((/=) 0) $ [ if x + y + z == 2020 then x * y * z else 0 | x <- inputData, y <- inputData, z <- inputData ]
          print solution2
          return ()
