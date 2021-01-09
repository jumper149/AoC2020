module Main

import Data.List
import Data.Nat
import Data.Nat.Views
import Data.Strings
import Data.Vect
import System.File

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

record Mask where
  constructor MkMask
  bits : Vect 36 (Maybe Bit)

Show Mask where
  show (MkMask bits) = "Mask " ++ show bits

record Mem where
  constructor MkMem
  address : Nat
  value : Vect 36 Bit

Show Mem where
  show (MkMem address value) = "Mem " ++ show address ++ " " ++ show value

Line : Type
Line = Either Mask Mem

Program : Type
Program = List Line

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

  digitsToBits : List Digit -> List Bit
  digitsToBits [] = []
  digitsToBits (D0 :: xs) = Zero :: digitsToBits xs
  digitsToBits (D1 :: xs) = One :: digitsToBits xs
  digitsToBits (_ :: xs) = ?cantCastDigitToBit

  natToBits : {n : Nat} -> Nat -> Vect n Bit
  natToBits x with (n)
    natToBits x | Z = []
    natToBits x | (S k) = if x `mod` 2 == 0
                             then Zero :: natToBits (x `div` 2)
                             else One :: natToBits (x `div` 2)

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
      grammarVal : Grammar (Token DataKind) True (Vect 36 Bit)
      grammarVal = (natToBits . digitsToNat) <$> (some $ match DKDigit)

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
              Left (Error err restToks) => print $ text <$> restToks
              Right (prog, rest) => print prog
         pure ()
  pure ()
