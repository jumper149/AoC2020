module Main

import Lightyear
import Lightyear.Char
import Lightyear.Position
import Lightyear.Strings as LS

data Seat = NoSeat
          | EmptySeat
          | OccupiedSeat

Show Seat where
  show NoSeat = "NoSeat"
  show EmptySeat = "EmptySeat"
  show OccupiedSeat = "OccupiedSeat"

data WaitingArea : Nat -> Nat -> Type where
  MkWaitingArea : Vect y (Vect x Seat) -> WaitingArea x y

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
pData {y=n} = ntimes n pLine <* eof

parseCountX : String -> Either String Nat
parseCountX = LS.parse $ colNo <$> (many pSeat *> getPosition)

parseCountY : String -> Either String Nat
parseCountY = ?r

parseWaitingArea : (x : Nat) -> (y : Nat) -> String -> Either String (WaitingArea x y)
parseWaitingArea x y str = MkWaitingArea <$> LS.parse pData str

main : IO ()
main = do
  inputData' <- readFile "./data"
  case inputData' of
       Right inputData => do
         eitherWaitingArea <- pure $ do
           x <- parseCountX inputData
           y <- parseCountY inputData
           -- parseWaitingArea x y inputData
           pure ()
         print $ inputData
  pure ()
