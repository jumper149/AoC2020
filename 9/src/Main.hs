import Data.Maybe (fromJust, isJust)
import qualified Data.Vector as V
import Text.Parsec

dataParser :: Parsec String () [Int]
dataParser = map read <$> endBy (many1 digit) newline <* eof

findSum :: Int -> [Int] -> Bool
findSum x xs = x `elem` sums
    where sums = fromJust <$> filter isJust [ if a == b then Nothing else Just (a + b) | a <- xs, b <- xs ]

findSums :: Int   -- how many numbers to overlook
         -> [Int] -- list of numbers
         -> [(Bool,Int)]
findSums n xs =
    if length xs > n
       then let firstN = take n xs
                currentN = xs !! n
             in (findSum currentN firstN , currentN) : findSums n (tail xs)
       else []

findWrongSum :: [(Bool,Int)] -> Int
findWrongSum ((True,_):xs) = findWrongSum xs
findWrongSum ((False,x):_) = x
findWrongSum [] = undefined

findContiguous :: Int          -- sum
               -> (Int,Int)    -- first and last number index
               -> V.Vector Int -- list of numbers
               -> V.Vector Int
findContiguous x (a,b) fullList
  | x == sumList = subList
  | x < sumList = findContiguous x (succ a,b) fullList
  | otherwise = findContiguous x (a,succ b) fullList
  where
      subList = V.slice a (b - a + 1) fullList
      sumList = sum subList

main :: IO ()
main = do
    file <- readFile "./data"
    let (Right intData) = parse dataParser "data" file
        wrongSum = findWrongSum $ findSums 25 intData
    print wrongSum
    let dataVec = V.fromList intData
        subList = findContiguous wrongSum (0,1) dataVec
        wrongSumMinMaxSum = minimum subList + maximum subList
    print wrongSumMinMaxSum
