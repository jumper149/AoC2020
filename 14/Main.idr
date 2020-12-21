module Main

import Data.List
import Data.Strings
import System.File

data Bit = Zero
         | One

Show Bit where
  show Zero = "0"
  show One = "1"

readBit : List Char -> List (Maybe Bit)
readBit [] = []
readBit ('X'::bits) = Nothing :: readBit bits
readBit ('0'::bits) = Just Zero :: readBit bits
readBit ('1'::bits) = Just One :: readBit bits
readBit (_::bits) = ?neverHappensReadBit

data ProgramLine = Mask (List (Maybe Bit))
                 | Mem Nat Nat

Show ProgramLine where
  show (Mask xs) = "Mask " ++ show xs
  show (Mem x y) = "Mem " ++ show x ++ " " ++ show y

sepMemAddrPart : List Char -> List Char
sepMemAddrPart (']'::_) = []
sepMemAddrPart (x::xs) = x :: sepMemAddrPart xs
sepMemAddrPart _ = ?neverHappensReadMemAddrPart

sepMemValuePart : List Char -> List Char
sepMemValuePart (' '::'='::' '::xs) = xs
sepMemValuePart (x::xs) = sepMemValuePart xs
sepMemValuePart _ = ?neverHappensSepMemValuePart

reversedToNat : List Char -> Nat
reversedToNat [] = 0
reversedToNat ('0'::xs) = 0 + 10 * reversedToNat xs
reversedToNat ('1'::xs) = 1 + 10 * reversedToNat xs
reversedToNat ('2'::xs) = 2 + 10 * reversedToNat xs
reversedToNat ('3'::xs) = 3 + 10 * reversedToNat xs
reversedToNat ('4'::xs) = 4 + 10 * reversedToNat xs
reversedToNat ('5'::xs) = 5 + 10 * reversedToNat xs
reversedToNat ('6'::xs) = 6 + 10 * reversedToNat xs
reversedToNat ('7'::xs) = 7 + 10 * reversedToNat xs
reversedToNat ('8'::xs) = 8 + 10 * reversedToNat xs
reversedToNat ('9'::xs) = 9 + 10 * reversedToNat xs
reversedToNat (_::xs) = ?neverHappensReveserdToNat

readProgramLine : List Char -> ProgramLine
readProgramLine ('m'::'a'::'s'::'k'::' '::'='::' '::bits) = Mask $ readBit bits
readProgramLine ('m'::'e'::'m'::'['::memStuff) =
  let addrPart = reversedToNat . reverse $ sepMemAddrPart memStuff
      valuePart = reversedToNat . reverse $ sepMemValuePart memStuff
  in Mem addrPart valuePart
readProgramLine _ = ?neverHappensReadProgramLine

main : IO ()
main = do
  inputData <- readFile "./data"
  case inputData of
       Left err => print err
       Right actualData => do
         let inputLines = unpack <$> lines actualData
             program = readProgramLine <$> inputLines
         print program
  pure ()
