{-# LANGUAGE TypeApplications #-}

main :: IO ()
main = do file <- readFile "./data"
          let inputData = read @ Integer <$> lines file
              solution = filter ((/=) 0) $ [ if x + y == 2020 then x * y else 0 | x <- inputData, y <- inputData ]
          print solution
          return ()
