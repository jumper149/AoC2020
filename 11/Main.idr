module Main

import Control.Monad.State
import Data.Fin
import Lightyear
import Lightyear.Char
import Lightyear.Position
import Lightyear.Strings as LS

-- data structures

data Seat = NoSeat
          | EmptySeat
          | OccupiedSeat

Eq Seat where
  (==) NoSeat NoSeat = True
  (==) NoSeat EmptySeat = False
  (==) NoSeat OccupiedSeat = False
  (==) EmptySeat NoSeat = False
  (==) EmptySeat EmptySeat = True
  (==) EmptySeat OccupiedSeat = False
  (==) OccupiedSeat NoSeat = False
  (==) OccupiedSeat EmptySeat = False
  (==) OccupiedSeat OccupiedSeat = True

Show Seat where
  show NoSeat = "NoSeat"
  show EmptySeat = "EmptySeat"
  show OccupiedSeat = "OccupiedSeat"

data WaitingArea : Nat -> Nat -> Type where
  MkWaitingArea : Vect y (Vect x Seat) -> WaitingArea x y

Eq (WaitingArea x y) where
  (==) (MkWaitingArea a) (MkWaitingArea b) = a == b

Show (WaitingArea x y) where
  show (MkWaitingArea seats) = show "WaitingArea " ++ show seats

record Coordinate (x : Nat) (y : Nat) where
  constructor MkCoordinate
  xCoord : Fin x
  yCoord : Fin y

Show (Fin n) where
  show {n=n} fin = "Fin " ++ show n ++ " " ++ show (finToNat fin)

Show (Coordinate x y) where
  show (MkCoordinate xCoord yCoord) = show "Coordinate "  ++ show xCoord ++ " " ++ show yCoord

-- parsing

pSeat : Parser Seat
pSeat = do
  s <- oneOf ".L#"
  pure $ case s of
              '.' => NoSeat
              'L' => EmptySeat
              '#' => OccupiedSeat

pLine : Parser (Vect x Seat)
pLine {x=n} = ntimes n pSeat <* newline

pData : Parser (Vect y (Vect x Seat))
pData {y=n} = ntimes n pLine <* eof

parseCountX : String -> Either String Nat
parseCountX = LS.parse $ (Prelude.Nat.pred . colNo) <$> (many pSeat *> getPosition)

parseCountY : String -> Either String Nat
parseCountY = LS.parse $ (Prelude.Nat.pred . lineNo) <$> (many ((many pSeat) <* newline) *> getPosition)

parseCountXY : String -> Either String (Nat,Nat)
parseCountXY str = do
  x <- parseCountX str
  y <- parseCountY str
  pure (x,y)

parseWaitingArea : (x : Nat) -> (y : Nat) -> String -> Either String (WaitingArea x y)
parseWaitingArea x y str = MkWaitingArea <$> LS.parse pData str

-- updating

-- Helper function for adjacentCoordinates.
-- This function looks complicated to cover n = 1, because succ and pred are annoying then.
adjacentFin : Fin n -> List (Fin n)
adjacentFin {n = Z} _ impossible
adjacentFin {n = S Z} FZ = []
adjacentFin {n = S Z} (FS _) impossible
adjacentFin FZ = [ FZ, succ FZ ]
adjacentFin (FS x) with ((FS x) == Data.Fin.last)
  adjacentFin (FS x) | False = [ pred (FS x), FS x, succ (FS x) ]
  adjacentFin (FS x) | True = [ pred (FS x), FS x ]

adjacentCoordinates : Coordinate x y -> List (Coordinate x y)
adjacentCoordinates (MkCoordinate xCoord yCoord) =
  [ MkCoordinate x y | x <- adjacentFin xCoord, y <- adjacentFin yCoord, x /= xCoord || y /= yCoord ]

getSeat : WaitingArea x y -> Coordinate x y -> Seat
getSeat (MkWaitingArea seats) (MkCoordinate xCoord yCoord) =
  index xCoord $ index yCoord seats

newSeat : WaitingArea x y -> Coordinate x y -> Seat
newSeat waitingArea coord = case oldSeat of
  NoSeat => NoSeat
  EmptySeat => if 0 == length occupiedNeighbors
                  then OccupiedSeat
                  else EmptySeat
  OccupiedSeat => if 4 <= length occupiedNeighbors
                     then EmptySeat
                     else OccupiedSeat
where
  oldSeat = getSeat waitingArea coord
  occupiedNeighbors : List Seat
  occupiedNeighbors = filter (== OccupiedSeat) $ getSeat waitingArea <$> adjacentCoordinates coord

allFins : Vect n (Fin n)
allFins {n=n} with (n)
  allFins {n=n} | Z = []
  allFins {n=n} | (S k) = FZ :: map FS allFins

allCoordinates : Vect y (Vect x (Coordinate x y))
allCoordinates = row <$> allFins where
  row y = MkCoordinate <$> allFins <*> pure y

updateWaitingArea : WaitingArea x y -> WaitingArea x y
updateWaitingArea waitingArea = MkWaitingArea $ map (newSeat waitingArea) <$> allCoordinates

-- updating (part 2)

data Direction = N
               | NE
               | E
               | SE
               | S
               | SW
               | W
               | NW

nextCoordInDirection : Coordinate x y -> Direction -> Maybe (Coordinate x y)
nextCoordInDirection {x=xx} {y=yy} (MkCoordinate x y) N = do
  newY <- integerToFin (finToInteger y + 1) yy
  pure $ MkCoordinate x newY
nextCoordInDirection {x=xx} {y=yy} (MkCoordinate x y) NE = do
  newY <- integerToFin (finToInteger y + 1) yy
  newX <- integerToFin (finToInteger x + 1) xx
  pure $ MkCoordinate newX newY
nextCoordInDirection {x=xx} {y=yy} (MkCoordinate x y) E = do
  newX <- integerToFin (finToInteger x + 1) xx
  pure $ MkCoordinate newX y
nextCoordInDirection {x=xx} {y=yy} (MkCoordinate x y) SE = do
  newY <- integerToFin (finToInteger y - 1) yy
  newX <- integerToFin (finToInteger x + 1) xx
  pure $ MkCoordinate newX newY
nextCoordInDirection {x=xx} {y=yy} (MkCoordinate x y) S = do
  newY <- integerToFin (finToInteger y - 1) yy
  pure $ MkCoordinate x newY
nextCoordInDirection {x=xx} {y=yy} (MkCoordinate x y) SW = do
  newY <- integerToFin (finToInteger y - 1) yy
  newX <- integerToFin (finToInteger x - 1) xx
  pure $ MkCoordinate newX newY
nextCoordInDirection {x=xx} {y=yy} (MkCoordinate x y) W = do
  newX <- integerToFin (finToInteger x - 1) xx
  pure $ MkCoordinate newX y
nextCoordInDirection {x=xx} {y=yy} (MkCoordinate x y) NW = do
  newY <- integerToFin (finToInteger y + 1) yy
  newX <- integerToFin (finToInteger x - 1) xx
  pure $ MkCoordinate newX newY

getNextSeatInDirection : WaitingArea x y -> Coordinate x y -> Direction -> Maybe Seat
getNextSeatInDirection waitingArea coord dir =
  getSeat waitingArea <$> nextCoordInDirection coord dir

isNextSeatInDirectionOccupied : WaitingArea x y -> Coordinate x y -> Direction -> Bool
isNextSeatInDirectionOccupied waitingArea coord dir with (getNextSeatInDirection waitingArea coord dir)
  isNextSeatInDirectionOccupied waitingArea coord dir | Nothing = False
  isNextSeatInDirectionOccupied waitingArea coord dir | (Just NoSeat) with (nextCoordInDirection coord dir)
    isNextSeatInDirectionOccupied waitingArea coord dir | (Just NoSeat) | Nothing = False -- actually impossible
    isNextSeatInDirectionOccupied waitingArea coord dir | (Just NoSeat) | (Just nextCoord) =
      isNextSeatInDirectionOccupied waitingArea nextCoord dir
  isNextSeatInDirectionOccupied waitingArea coord dir | (Just EmptySeat) = False
  isNextSeatInDirectionOccupied waitingArea coord dir | (Just OccupiedSeat) = True

countNextOccupiedSeats : WaitingArea x y -> Coordinate x y -> Nat
countNextOccupiedSeats waitingArea coord =
  length $ filter id $ isNextSeatInDirectionOccupied waitingArea coord <$> [N, NE, E, SE, S, SW, W, NW]

newSeat2 : WaitingArea x y -> Coordinate x y -> Seat
newSeat2 waitingArea coord = case oldSeat of
  NoSeat => NoSeat
  EmptySeat => if 0 == occupiedNeighbors
                  then OccupiedSeat
                  else EmptySeat
  OccupiedSeat => if 5 <= occupiedNeighbors
                     then EmptySeat
                     else OccupiedSeat
where
  oldSeat = getSeat waitingArea coord
  occupiedNeighbors : Nat
  occupiedNeighbors = countNextOccupiedSeats waitingArea coord

updateWaitingArea2 : WaitingArea x y -> WaitingArea x y
updateWaitingArea2 waitingArea = MkWaitingArea $ map (newSeat2 waitingArea) <$> allCoordinates

-- counting

countOccupiedSeats : WaitingArea x y -> Nat
countOccupiedSeats (MkWaitingArea seats) =
  length $ elemIndices OccupiedSeat $ concat seats

-- main

upd : StateT (WaitingArea x y) IO ()
upd = do
  wa <- get
  lift $ print $ countOccupiedSeats wa
  lift $ putStr "\n"
  --let waNew = updateWaitingArea wa
  let waNew = updateWaitingArea2 wa
  put waNew
  upd

main : IO ()
main = do
  inputData' <- readFile "./data"
  case inputData' of
       Right inputData => case parseCountXY inputData of
                               Right (x,y) => case parseWaitingArea x y inputData of
                                                   Right w0 => do
                                                     runStateT upd w0
                                                     pure ()
