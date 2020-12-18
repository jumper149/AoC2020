import Control.Monad.State.Strict
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import qualified Data.Vector as V

differences :: [Integer] -> [Integer]
differences [] = undefined
differences [_] = []
differences (x0:x1:xs) = (x1 - x0) : differences (x1:xs)

isValidChain' :: [Integer] -> Bool
isValidChain' [] = undefined
isValidChain' [_] = True
isValidChain' (x1:x2:xs) = condition && isValidChain' (x2:xs)
    where diff = x2 - x1
          condition = diff > 0 && diff < 4

isValidChain :: V.Vector Integer -> Bool
isValidChain = isValidChain' . V.toList

type Selector = V.Vector Bool

initialSelector :: V.Vector Integer -> Selector
initialSelector is = V.replicate (V.length is) True

applySelector :: V.Vector Integer -> Selector -> V.Vector Integer
applySelector is bs = fromMaybeVec $ V.zipWith f bs is
    where f False _ = Nothing
          f True i = Just i
          fromMaybeVec = V.fromList . catMaybes . V.toList

checkSelector :: V.Vector Integer -> Selector -> Bool
checkSelector is bs = isValidChain $ applySelector is bs

trySelectors :: V.Vector Integer -> Selector -> State (S.Set Selector) ()
trySelectors is bs = when currentSelectorCheck $ do
       ss <- get
       if bs `S.member` ss
          then return ()
          else do modify $ S.insert bs
                  traverse_ (trySelectors is) newBs
    where currentSelectorCheck = checkSelector is bs
          newBs = catMaybes [ generateSelector j bs | j <- [1..(length is - 2)]] -- Leave out first and last (outlet and input)

generateSelector :: Int -> Selector -> Maybe Selector
generateSelector j bs = if currentB
                           then Just $ bs V.// [(j,False)]
                           else Nothing
    where currentB = bs V.! j

main :: IO ()
main = do
    file <- readFile "./data"
    let joltages' = S.fromList $ read <$> lines file
        joltages = S.insert 0 $ S.insert (maximum joltages' + 3) joltages'
        diffList = differences $ S.toAscList joltages
        numberOf3 = length $ filter (3 ==) diffList
        numberOf1 = length $ filter (1 ==) diffList
    print $ numberOf1 * numberOf3
    let joltagesVec = V.fromList $ S.toAscList joltages
        selectors = execState (trySelectors joltagesVec (initialSelector joltagesVec)) S.empty
    print $ S.size selectors
    return ()
