import Data.List (sort)
import Control.Monad.Writer

differences :: [Integer] -> [Integer]
differences [] = undefined
differences [_] = []
differences (x0:x1:xs) = (x1 - x0) : differences (x1:xs)

inputDifferencesUnsafe :: [Integer] -> Differences
inputDifferencesUnsafe = map f
    where f 1 = D1
          f 3 = D3
          f _ = undefined

type Differences = [Difference]

data Difference =
    D1
      | D3
      deriving (Enum, Eq, Ord, Read, Show)

    {-
     - count D1's until next D3
     - 0 -> 1
     - 1 -> 1
     - 2 -> 2
     - 3 -> 4
     - 4 -> 7
     - 5 -> 13
     - 6 -> 24
     -
     - this led me to the tribonacci series:
     - https://oeis.org/search?q=1%2C1%2C2%2C4%2C7%2C13%2C24&sort=&language=english&go=Search
     -}

tribs :: [Integer]
tribs = 1 : 1 : 2 : [ (tribs !! i) + (tribs !! succ i) + (tribs !! succ (succ i)) | i <- [0..]]

trib :: Integer -> Integer
trib n = tribs !! fromEnum n

countGroupOfD1 :: [Difference] -> (Integer,[Difference])
countGroupOfD1 [] = undefined
countGroupOfD1 (D3:ds) = (0,ds)
countGroupOfD1 (D1:[]) = (1,[])
countGroupOfD1 (D1:ds) = let (n,rest) = countGroupOfD1 ds
                          in (succ n,rest)

countGroupsOfD1 :: [Difference] -> Writer [Integer] ()
countGroupsOfD1 ds = do
    let (n,rest) = countGroupOfD1 ds
    tell $ pure n
    if rest == []
       then return ()
       else countGroupsOfD1 rest

main :: IO ()
main = do
    file <- readFile "./data"
    let joltages' = read <$> lines file
        joltages = sort $ 0 : joltages' ++ [(maximum joltages' + 3)]
        diffList = differences joltages
        numberOf3 = length $ filter (3 ==) diffList
        numberOf1 = length $ filter (1 ==) diffList
    print $ numberOf1 * numberOf3
    print $ product $ map trib $ execWriter $ countGroupsOfD1 $ inputDifferencesUnsafe $ diffList
    return ()
