module Main

-- base
import Control.Monad.State
import Data.List
import System.File

-- contrib
import Data.SortedMap
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

  minus : Nat -> Nat -> Maybe Nat
  minus Z Z = Just Z
  minus (S k) Z = Just $ S k
  minus Z (S k) = Nothing
  minus (S k) (S k') = k `minus` k'

  times : List Nat -> Maybe Nat
  times ns = 2020 `minus` length ns

  export
  turns : List Nat -> Maybe Nat
  turns ns = (doTimes <$> times ns <*> pure turn <*> pure ns) >>= head'

  times2 : List Nat -> Maybe Nat
  times2 ns = 30000000 `minus` length ns

namespace Part2
  record Game where
    constructor MkGame
    lookupTable : SortedMap Nat Nat
    lastNumber : Nat
    counter : Nat

  Show Game where
    show (MkGame lookupTable lastNumber counter) =
      "Game { lookupTable = " ++ show lookupTable
            ++ ", lastNumber = " ++ show lastNumber
            ++ ", counter = " ++ show counter
            ++ " }"

  scriptedTurn : Monad m => Nat -> StateT Game m ()
  scriptedTurn n = do
    game <- get
    let newLookupTable = (1 +) <$> insert game.lastNumber 0 game.lookupTable
        newCounter = 1 + game.counter
    put $ record { lookupTable = newLookupTable
                 , lastNumber = n
                 , counter = newCounter
               } game

  turn : Monad m => StateT Game m ()
  turn = do
    game <- get
    let mbDistance = lookup game.lastNumber game.lookupTable
        newLookupTable = (1 +) <$> insert game.lastNumber 0 game.lookupTable
        newCounter = 1 + game.counter
    put $ record { lookupTable = newLookupTable
                 , lastNumber = case mbDistance of
                                     Nothing => 0
                                     Just n => n
                 , counter = newCounter
               } game

  log : StateT Game IO ()
  log = do
    game <- get
    if natToInteger game.counter `mod` 10000 == 0
       then lift $ putStrLn $ show $ game.lookupTable
       else pure ()

  export
  turns : Nat -> StateT Game IO ()
  turns n = do
    n' <- gets counter
    --log
    if n == n'
       then do
         game <- get
         lift $ putStrLn $ show $ game.lastNumber
       else do
         turn
         turns n

  export
  initialGame : List Nat -> Game
  initialGame xs = removeWrongZero $ execState scriptedTurns startVal where
    removeWrongZero : Game -> Game
    removeWrongZero (MkGame lookupTable lastNumber counter) =
      MkGame newLookupTable lastNumber counter where
        newLookupTable : SortedMap Nat Nat
        newLookupTable = case lookup 0 lookupTable of
                              Nothing => lookupTable
                              Just n => if n == counter
                                           then delete 0 lookupTable
                                           else lookupTable
    scriptedTurns : Monad m => StateT Game m ()
    scriptedTurns = sequence_ $ map scriptedTurn $ xs
    startVal : Game
    startVal = MkGame empty 0 0 -- TODO: doesn't work unless the first number is 0; get's fixed in removeWrongZero

main : IO ()
main = do
  inputData <- readFile "./data"
  case inputData of
       Left err => print err
       Right dat => do
         let startingNumbers = readInput dat
         --traverse_ (putStrLn . show) $ turns startingNumbers
         let startingNumbers' = reverse startingNumbers
             maxCounter = 30000000
         _ <- runStateT (turns 300000) (initialGame startingNumbers')
         pure ()
  pure ()
