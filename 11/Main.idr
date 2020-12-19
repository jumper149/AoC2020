module Main

import Lightyear
import Lightyear.Strings as LS
import Lightyear.Char

data Seat = NoSeat
          | EmptySeat
          | OccupiedSeat

Show Seat where
  show NoSeat = "NoSeat"
  show EmptySeat = "EmptySeat"
  show OccupiedSeat = "OccupiedSeat"

data WaitingArea : Nat -> Nat -> Type where
  MkWaitingArea : Vect y (Vect x Seat) -> WaitingArea x y

                  --(Vect x (Vect y Seat))

Show (WaitingArea x y) where
  show (MkWaitingArea seats) = show "WaitingArea " ++ show seats

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
pData {y=n} = ntimes n pLine

pWaitingArea : Parser (WaitingArea 3 3)
pWaitingArea = MkWaitingArea <$> pData <* eof

main : IO ()
main = do
  inputData <- readFile "./data"
  case inputData of
       Right inputData' => do let waitingArea = LS.parse pWaitingArea inputData'
                              print $ show waitingArea
                              pure ()
  pure ()
