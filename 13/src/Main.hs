{-# LANGUAGE RecordWildCards #-}

import Control.Monad.State
import Data.Functor (($>))
import Data.Maybe (catMaybes)
import qualified Data.Vector as V
import Text.Parsec hiding (State)
import Text.Parsec.String

data BusInfo = BusInfo
    { myEarliestDeparture :: Integer
    , maybeBusIDs :: [Maybe Integer]
    }
    deriving (Eq, Ord, Read, Show)

pBusInfo :: Parser BusInfo
pBusInfo = do
    myEarliestDeparture <- read <$> many digit <* newline
    maybeBusIDs <- pMaybeIntegers <* newline <* eof
    return BusInfo {..}
    where pInteger = Just . read <$> many1 digit
          pX = char 'x' $> Nothing
          pMaybeInteger = try pInteger <|> pX
          pMaybeIntegers = pMaybeInteger `sepBy` char ','

newtype BusIDs = BusIDs (V.Vector Integer)
    deriving (Eq, Ord, Read, Show)

data BusState = BusState
    { timestamp :: Integer
    , busCountdown :: V.Vector Integer
    }
    deriving (Eq, Ord, Read, Show)

initialBusState :: BusIDs -> BusState
initialBusState (BusIDs countdown) =
    BusState { timestamp = 0
             , busCountdown = const 0 <$> countdown
             }

simulateTimestep :: BusIDs -> State BusState ()
simulateTimestep (BusIDs busIDs) = do
    bs <- get
    put bs { timestamp = succ $ timestamp bs
           , busCountdown = V.zipWith countDown (busCountdown bs) busIDs
           }
    where countDown x bId = if x == 0
                               then pred bId
                               else pred x

getArrivingBuses :: BusIDs -> State BusState [Integer]
getArrivingBuses (BusIDs busIDs) = do
    BusState {..} <- get
    return $ catMaybes $ V.toList $ V.zipWith selectArrived busCountdown busIDs
    where selectArrived 0 bId = Just bId
          selectArrived _ _ = Nothing

simulateWaiting :: BusIDs -> Integer -> State BusState ()
simulateWaiting bIds finalTimestamp = do
    bs <- get
    if timestamp bs == finalTimestamp
       then return ()
       else do simulateTimestep bIds
               simulateWaiting bIds finalTimestamp

takeNextBus :: BusIDs -> State BusState [Integer]
takeNextBus bIds = do
    arrivingBuses <- getArrivingBuses bIds
    if null arrivingBuses
       then do simulateTimestep bIds
               takeNextBus bIds
       else return arrivingBuses

multiplySolution :: Integer -- bus ID
                 -> Integer -- earliest departure
                 -> BusState -- actual departure bus state
                 -> Integer
multiplySolution bId dep0 bs = (timestamp bs - dep0) * bId

main :: IO ()
main = do
    eitherBusInfo <- parseFromFile pBusInfo "./data"
    case eitherBusInfo of
      Left msg -> print msg
      Right busInfo@BusInfo {..} -> do
          print busInfo
          let busIDs = BusIDs . V.fromList $ catMaybes maybeBusIDs
          print busIDs
          let newBusState = execState (simulateWaiting busIDs myEarliestDeparture) (initialBusState busIDs)
          print newBusState
          let ([busID],busStateToTake) = runState (takeNextBus busIDs) newBusState
          print $ multiplySolution busID myEarliestDeparture busStateToTake
    return ()
