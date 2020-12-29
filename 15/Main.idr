module Main

-- base
import Data.List
import System.File

-- contrib
import Text.Lexer
import Text.Lexer.Core
import Text.Parser
import Text.Parser.Core

namespace Input
  data Token = TComma
             | TUnknown
             | TNat Nat

  readDigit : Char -> Maybe Nat
  readDigit '0' = Just 0
  readDigit '1' = Just 1
  readDigit '2' = Just 2
  readDigit '3' = Just 3
  readDigit '4' = Just 4
  readDigit '5' = Just 5
  readDigit '6' = Just 6
  readDigit '7' = Just 7
  readDigit '8' = Just 8
  readDigit '9' = Just 9
  readDigit _ = Nothing

  combineReversedDigits : List Nat -> Nat
  combineReversedDigits [] = 0
  combineReversedDigits (n :: ns) = n + 10 * combineReversedDigits ns

  reverseNaturalGrammar : Grammar Char False Nat
  reverseNaturalGrammar =
    map combineReversedDigits $ many $ terminal "Can't parse digit" readDigit

  readTNat : String -> Token
  readTNat str = do
    let eithNat = parse reverseNaturalGrammar $ unpack $ reverse str
    case eithNat of
         Left _ => TUnknown
         Right (nat,_) => TNat nat

  toTokenData : String -> List (TokenData Token)
  toTokenData str = fst $
                    [ (is ',' , const TComma)
                    , (digits , readTNat)
                    ] `lex` str

  export
  readInput : String -> List Nat
  readInput str = reverse $ toNatList $ map tok $ toTokenData str where
    toNatList : List Token -> List Nat
    toNatList [] = []
    toNatList (TComma :: xs) = toNatList xs
    toNatList (TUnknown :: xs) = toNatList xs
    toNatList (TNat n :: xs) = n :: toNatList xs

namespace Part1

  findIndex : Eq a => a -> List a -> Maybe Nat
  findIndex y [] = Nothing
  findIndex y (x :: xs) =
    if x == y
       then Just 0
       else (1 +) <$> findIndex y xs

  nextNumber : List Nat -> Nat
  nextNumber [] = 0
  nextNumber (x :: xs) =
    case findIndex x xs of
         Nothing => 0
         Just n => 1 + n

  turn : List Nat -> List Nat
  turn ns = nextNumber ns :: ns

  doTimes : Nat -> (a -> a) -> a -> a
  doTimes Z _ x = x
  doTimes (S k) f x = doTimes k f $ f x

  times : List Nat -> Maybe Nat
  times ns = 2020 `minus` length ns where
    minus : Nat -> Nat -> Maybe Nat
    minus Z Z = Just Z
    minus (S k) Z = Just $ S k
    minus Z (S k) = Nothing
    minus (S k) (S k') = k `minus` k'

  export
  turns : List Nat -> Maybe Nat
  turns ns = (doTimes <$> times ns <*> pure turn <*> pure ns) >>= head'

main : IO ()
main = do
  inputData <- readFile "./data"
  case inputData of
       Left err => print err
       Right dat => do
         let startingNumbers = readInput dat
         traverse_ print $ turns startingNumbers
  pure ()
