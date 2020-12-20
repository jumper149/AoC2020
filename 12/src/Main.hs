import Control.Monad.State
import Data.Foldable (traverse_)
import Text.Parsec hiding (State)
import Text.Parsec.String

data Direction = North
               | South
               | East
               | West
               deriving (Enum, Eq, Ord, Read, Show)

data Turn = TurnLeft
          | TurnRight
          deriving (Enum, Eq, Ord, Read, Show)

data ActionPrefix = Direction Direction
                  | Turn Turn
                  | MoveForward
                  deriving (Eq, Ord, Read, Show)

data Action = Action ActionPrefix Integer
    deriving (Eq, Ord, Read, Show)

pAction :: Parser Action
pAction = do
    firstChar <- oneOf "NSEWLRF"
    let actionPrefix = case firstChar of
                         'N' -> Direction North
                         'S' -> Direction South
                         'E' -> Direction East
                         'W' -> Direction West
                         'L' -> Turn TurnLeft
                         'R' -> Turn TurnRight
                         'F' -> MoveForward
                         _ -> undefined
    secondInteger <- many1 digit
    let actionValue = read secondInteger
    return $ Action actionPrefix actionValue

pActions :: Parser [Action]
pActions = many (pAction <* newline) <* eof

newtype ShipDirection = ShipDirection Integer
    deriving (Enum, Eq, Ord, Read, Show)

changeShipDirection :: ShipDirection -> ShipDirection -> ShipDirection
changeShipDirection (ShipDirection x) (ShipDirection y) = ShipDirection $ (x + y) `mod` 360

instance Bounded ShipDirection where
    minBound = ShipDirection 0
    maxBound = ShipDirection 359

data ShipState = ShipState
    { xCoord :: Integer
    , yCoord :: Integer
    , shipDirection :: ShipDirection
    }
    deriving (Eq, Ord, Read, Show)

simulateAction :: Action -> State ShipState ()
simulateAction (Action actionPrefix amount) = do
    ss <- get
    let (x,y) = (xCoord ss,yCoord ss)
        shipDir = shipDirection ss
    case actionPrefix of
      Direction direction -> do
          let (newX,newY) = case direction of
                              North -> (x,y+amount)
                              South -> (x,y-amount)
                              East -> (x+amount,y)
                              West -> (x-amount,y)
          put ss { xCoord = newX, yCoord = newY }
      Turn degrees -> do
          let newShipDir = case degrees of
                             TurnLeft -> changeShipDirection shipDir $ ShipDirection (-amount)
                             TurnRight -> changeShipDirection shipDir $ ShipDirection amount
          put ss { shipDirection = newShipDir }
      MoveForward -> do
          let (newX,newY) = case shipDir of
                              ShipDirection 0 -> (x,y+amount)
                              ShipDirection 90 -> (x+amount,y)
                              ShipDirection 180 -> (x,y-amount)
                              ShipDirection 270 -> (x-amount,y)
                              _ -> undefined
          put ss { xCoord = newX, yCoord = newY }

simulateActions :: [Action] -> State ShipState ()
simulateActions = traverse_ simulateAction

manhattanNorm :: ShipState -> Integer
manhattanNorm ShipState { xCoord = x, yCoord = y } = abs x + abs y

data ShipStateWaypoint = ShipStateWaypoint
    { xShip :: Integer
    , yShip :: Integer
    , xWaypoint :: Integer
    , yWaypoint :: Integer
    }
    deriving (Eq, Ord, Read, Show)

turnCoordinates :: Integer -> (Integer,Integer) -> (Integer,Integer)
turnCoordinates 0 (x,y) = (x,y)
turnCoordinates 90 (x,y) = (y,-x)
turnCoordinates 180 (x,y) = (-x,-y)
turnCoordinates 270 (x,y) = (-y,x)
turnCoordinates _ _ = undefined

simulateActionWaypoint :: Action -> State ShipStateWaypoint ()
simulateActionWaypoint (Action actionPrefix amount) = do
    ss <- get
    let (xs,ys) = (xShip ss,yShip ss)
        (xw,yw) = (xWaypoint ss,yWaypoint ss)
    case actionPrefix of
      Direction direction -> do
          let (newXw,newYw) = case direction of
                                North -> (xw,yw+amount)
                                South -> (xw,yw-amount)
                                East -> (xw+amount,yw)
                                West -> (xw-amount,yw)
          put ss { xWaypoint = newXw, yWaypoint = newYw }
      Turn degrees -> do
          let (newXw,newYw) = case degrees of
                             TurnLeft -> turnCoordinates ((-amount) `mod` 360) (xw,yw)
                             TurnRight -> turnCoordinates (amount `mod` 360) (xw,yw)
          put ss { xWaypoint = newXw, yWaypoint = newYw }
      MoveForward -> do
          let (newXs,newYs) = (xs + amount * xw , ys + amount * yw)
          put ss { xShip = newXs, yShip = newYs }

simulateActionsWaypoint :: [Action] -> State ShipStateWaypoint ()
simulateActionsWaypoint = traverse_ simulateActionWaypoint

manhattanNormWaypoint :: ShipStateWaypoint -> Integer
manhattanNormWaypoint ShipStateWaypoint { xShip = x, yShip = y } = abs x + abs y

main :: IO ()
main = do
    eitherActions <- parseFromFile pActions "./data"
    case eitherActions of
      Left err -> print err
      Right actions -> do
          let initState = ShipState { xCoord = 0
                                    , yCoord = 0
                                    , shipDirection = ShipDirection 90
                                    }
          print $ manhattanNorm $ execState (simulateActions actions) initState
          let initStateWaypoint = ShipStateWaypoint { xShip = 0
                                                    , yShip = 0
                                                    , xWaypoint = 10
                                                    , yWaypoint = 1
                                                    }
          print $ manhattanNormWaypoint $ execState (simulateActionsWaypoint actions) initStateWaypoint
    return ()
