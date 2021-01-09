module Main

import Control.Monad.State
import Data.List
import Data.Nat
import Data.Nat.Views
import Data.Strings
import Data.Vect
import System.File

import Data.SortedMap
import Text.Lexer
import Text.Lexer.Core
import Text.Parser
import Text.Parser.Core
import Text.Token

data Bit = Zero
         | One

Show Bit where
  show Zero = "Zero"
  show One = "One"

Bits : Type
Bits = Vect 36 Bit

record Mask where
  constructor MkMask
  bits : Vect 36 (Maybe Bit)

Show Mask where
  show (MkMask bits) = "Mask " ++ show bits

record Mem where
  constructor MkMem
  address : Nat
  value : Bits

Show Mem where
  show (MkMem address value) = "Mem " ++ show address ++ " " ++ show value

Line : Type
Line = Either Mask Mem

Program : Type
Program = List Line

Memory : Type
Memory = SortedMap Nat Bits

applyMask : Mask -> Bits -> Bits
applyMask (MkMask maskBits) valBits = f <$> maskBits <*> valBits where
  f : Maybe Bit -> Bit -> Bit
  f Nothing x = x
  f (Just x) _ = x

initState : (Mask, Memory)
initState = (MkMask $ pure Nothing, empty)

progLine : Line -> State (Mask,Memory) ()
progLine (Left mask) = do
  (oldMask, memory) <- get
  put (mask, memory)
progLine (Right (MkMem address value)) = do
  (mask, oldMemory) <- get
  let memory = insert address (applyMask mask value) oldMemory
  put (mask, memory)

bitsToNat : Bits -> Nat
bitsToNat = bitsToNat' . reverse where
  bitsToNat' : Vect n Bit -> Nat
  bitsToNat' [] = 0
  bitsToNat' (One :: xs) = 1 + 2 * bitsToNat' xs
  bitsToNat' (Zero :: xs) = 0 + 2 * bitsToNat' xs

namespace Parsing
  data DataKind = DKMask
                | DKMem
                | DKBracketOpen
                | DKBracketClose
                | DKSpace
                | DKEquals
                | DKNewline
                | DKNoBit
                | DKDigit

  Eq DataKind where
    (==) DKMask DKMask = True
    (==) DKMem DKMem = True
    (==) DKBracketOpen DKBracketOpen = True
    (==) DKBracketClose DKBracketClose = True
    (==) DKSpace DKSpace = True
    (==) DKEquals DKEquals = True
    (==) DKNewline DKNewline = True
    (==) DKNoBit DKNoBit = True
    (==) DKDigit DKDigit = True
    (==) _ _ = False

  export
  Show DataKind where
    show DKMask = "DKMask"
    show DKMem = "DKMem"
    show DKBracketOpen = "DKBracketOpen"
    show DKBracketClose = "DKBracketClose"
    show DKSpace = "DKSpace"
    show DKEquals = "DKEquals"
    show DKNewline = "DKNewline"
    show DKNoBit = "DKNoBit"
    show DKDigit = "DKDigit"

  data Digit = D0
             | D1
             | D2
             | D3
             | D4
             | D5
             | D6
             | D7
             | D8
             | D9

  Cast String Digit where
    cast "0" = D0
    cast "1" = D1
    cast "2" = D2
    cast "3" = D3
    cast "4" = D4
    cast "5" = D5
    cast "6" = D6
    cast "7" = D7
    cast "8" = D8
    cast "9" = D9
    cast _ = ?cantCastNonDigit

  TokenKind DataKind where
    TokType DKMask = ()
    TokType DKMem = ()
    TokType DKBracketOpen = ()
    TokType DKBracketClose = ()
    TokType DKSpace = ()
    TokType DKEquals = ()
    TokType DKNewline = ()
    TokType DKNoBit = ()
    TokType DKDigit = Digit
    tokValue DKMask _ = ()
    tokValue DKMem _ = ()
    tokValue DKBracketOpen _ = ()
    tokValue DKBracketClose _ = ()
    tokValue DKSpace _ = ()
    tokValue DKEquals _ = ()
    tokValue DKNewline _ = ()
    tokValue DKNoBit _ = ()
    tokValue DKDigit str = cast str

  export
  tokenMap : TokenMap $ Token DataKind
  tokenMap = toTokenMap
    [ (exact "mask", DKMask)
    , (exact "mem", DKMem)
    , (is '[', DKBracketOpen)
    , (is ']', DKBracketClose)
    , (newline, DKNewline)
    , (space, DKSpace)
    , (is '=', DKEquals)
    , (is 'X', DKNoBit)
    , (digit, DKDigit)
    ]

  digitsToNat : List Digit -> Nat
  digitsToNat [] = 0
  digitsToNat (x :: xs) = digitToNat x + 10 * digitsToNat xs where
    digitToNat : Digit -> Nat
    digitToNat D0 = 0
    digitToNat D1 = 1
    digitToNat D2 = 2
    digitToNat D3 = 3
    digitToNat D4 = 4
    digitToNat D5 = 5
    digitToNat D6 = 6
    digitToNat D7 = 7
    digitToNat D8 = 8
    digitToNat D9 = 9

  export
  integerToBits : {n : Nat} -> Integer -> Vect n Bit
  integerToBits x with (n)
    integerToBits x | Z = []
    integerToBits x | (S k) = if x `mod` 2 == 0
                             then Zero :: integerToBits (x `div` 2)
                             else One :: integerToBits (x `div` 2)

  grammarMem : Grammar (Token DataKind) True Mem
  grammarMem = do
    match DKMem
    match DKBracketOpen
    digitsAddress <- some $ match DKDigit
    let address = digitsToNat $ reverse digitsAddress
    match DKBracketClose
    match DKSpace
    match DKEquals
    match DKSpace
    MkMem address <$> grammarVal where
      grammarVal : Grammar (Token DataKind) True Bits
      grammarVal = (reverse . integerToBits . natToInteger . digitsToNat . reverse) <$> (some $ match DKDigit)

  countExactly : (n : Nat) -> (p : Grammar tok True a) -> Grammar tok (isSucc n) (Vect n a)
  countExactly Z p = Empty []
  countExactly (S k) p = [| p :: countExactly k p |]

  grammarMbBit : Grammar (Token DataKind) True (Maybe Bit)
  grammarMbBit = Just <$> grammarDig <|> grammarX where
    grammarDig : Grammar (Token DataKind) True Bit
    grammarDig = do
      n <- match DKDigit
      f n where
        f : Digit -> Grammar (Token DataKind) False Bit
        f D0 = pure Zero
        f D1 = pure One
        f _ = fail "Can't parser bit other than 0 or 1"
    grammarX : Grammar (Token DataKind) True (Maybe Bit)
    grammarX = match DKNoBit *> pure Nothing

  grammarMask : Grammar (Token DataKind) True Mask
  grammarMask = do
    match DKMask
    match DKSpace
    match DKEquals
    match DKSpace
    bits <- countExactly 36 $ grammarMbBit
    pure $ MkMask bits

  grammarLine : Grammar (Token DataKind) True Line
  grammarLine = (Left <$> grammarMask <|> Right <$> grammarMem) <* match DKNewline

  export
  grammarData : Grammar (Token DataKind) True Program
  grammarData = some grammarLine <* eof

main : IO ()
main = do
  inputData <- readFile "./data"
  case inputData of
       Left err => print err
       Right actualData => do
         let (lexed, debugLexed) = lex tokenMap actualData
             parsed = parse grammarData $ tok <$> lexed
         case parsed of
              Left (Error err restToks) => do
                print $ text <$> restToks
                print err
              Right (prog, rest) => do
                let (endMask, endMemory) = execState initState (traverse_ progLine prog)
                print $ sum $ bitsToNat <$> endMemory
         pure ()
  pure ()
