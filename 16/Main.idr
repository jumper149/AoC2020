module Main

-- base
import Data.List
import Data.Strings
import System.File

--contrib
import Text.Lexer
import Text.Lexer.Core
import Text.Parser
import Text.Parser.Core

record FieldConstraint where
  constructor MkFieldConstraint
  field : String
  isValid : Nat -> Bool

Show FieldConstraint where
  show (MkFieldConstraint field isValid) =
    "FieldConstraint " ++ show field ++ " lambda"

namespace Parsing
  data DataKind = DKWord
                | DKNat
                | DKSpace
                | DKColon
                | DKMinus
                | DKOr
                | DKComma
                | DKNewline

  Eq DataKind where
    DKWord == DKWord = True
    DKNat == DKNat = True
    DKSpace == DKSpace = True
    DKColon == DKColon = True
    DKMinus == DKMinus = True
    DKOr == DKOr = True
    DKComma == DKComma = True
    DKNewline == DKNewline = True
    _ == _ = True

  Show DataKind where
    show DKWord = "DKWord"
    show DKNat = "DKNat"
    show DKSpace = "DKSpace"
    show DKColon = "DKColon"
    show DKMinus = "DKMinus"
    show DKOr = "DKOr"
    show DKComma = "DKComma"
    show DKNewline = "DKNewline"

  TokenKind DataKind where
    TokType DKWord = String
    TokType DKNat = Nat
    TokType _ = ()
    tokValue DKWord x = x
    tokValue DKNat x = integerToNat $ cast x
    tokValue DKSpace _ = ()
    tokValue DKColon _ = ()
    tokValue DKMinus _ = ()
    tokValue DKOr _ = ()
    tokValue DKComma _ = ()
    tokValue DKNewline _ = ()

  export
  tokenMap : TokenMap $ Token DataKind
  tokenMap = toTokenMap
    [ (is ':', DKColon)
    , (is '-', DKMinus)
    , (exact "or", DKOr)
    , (is ',', DKComma)
    , (newline, DKNewline)
    , (spaces, DKSpace)
    , (alphas, DKWord)
    , (digits, DKNat)
    ]

  -- NOTE TO SELF!!!
  -- It's important to keep the consuming Bool of Grammar True, when one want to keep parsing!!!

  -- Needs to end with DKColon
  wordsGrammar : Grammar (Token DataKind) True (List String)
  wordsGrammar = do
    w <- match DKWord
    n <- peek
    case kind n of
         DKSpace => do
           match DKSpace
           ws <- wordsGrammar
           pure $ w :: ws
         DKColon => do
           match DKColon
           pure [ w ]
         _ => fail "Expecting words"

  fieldConstraintGrammar : Grammar (Token DataKind) True FieldConstraint
  fieldConstraintGrammar = do
    field <- unwords <$> wordsGrammar
    match DKSpace
    xMin <- match DKNat
    match DKMinus
    xMax <- match DKNat
    match DKSpace
    match DKOr
    match DKSpace
    yMin <- match DKNat
    match DKMinus
    yMax <- match DKNat
    match DKNewline
    let constraint : Nat -> Bool
        constraint a = a >= xMin && a <= xMax || a >= yMin && a <= yMax
    pure $ MkFieldConstraint field constraint

  ticketGrammar : Grammar (Token DataKind) True (List Nat)
  ticketGrammar = do
    n <- match DKNat
    o <- peek
    case kind o of
         DKComma => do
           match DKComma
           ns <- ticketGrammar
           pure $ n :: ns
         DKNewline => do
           match DKNewline
           pure [ n ]
         _ => fail "Expecting Nats"

  export
  dataGrammar : Grammar (Token DataKind) True (List FieldConstraint, List Nat, List $ List Nat)
  dataGrammar = do
    fieldConstraints <- many fieldConstraintGrammar
    match DKNewline
    wordsGrammar -- match "your ticket:"
    match DKNewline
    myTicket <- ticketGrammar
    match DKNewline
    wordsGrammar -- match "nearby tickets:"
    match DKNewline
    nearbyTickets <- many ticketGrammar
    eof
    pure (fieldConstraints, myTicket, nearbyTickets)

namespace Part1
  zipper : List (a -> b) -> List a -> List b
  zipper [] ys = []
  zipper xs [] = []
  zipper (x::xs) (y::ys) = x y :: zipper xs ys

  export
  countTicketErrors : (fieldConstraints : List FieldConstraint) -> (fields : List Nat) -> List Nat
  countTicketErrors fieldConstraints fields =
    map fst $ filter (not . snd) $ map g $ f (isValid <$> fieldConstraints) <$> fields where
      f : List (Nat -> Bool) -> Nat -> (Nat,List Bool)
      f fs n = (n,fs <*> pure n)
      any : List Bool -> Bool
      any [] = False
      any (True::xs) = True
      any (False::xs) = any xs
      g : (Nat,List Bool) -> (Nat,Bool)
      g (n,bs) = (n, any bs)

  export
  removeWhenTicketError : (fieldConstraints : List FieldConstraint) -> (fieldss : List (List Nat)) -> List (List Nat)
  removeWhenTicketError fieldConstraints fieldss =
    catMaybes $ zipWith removeWhenErr errss fieldss where
      errss : List $ List Nat
      errss = countTicketErrors fieldConstraints <$> fieldss
      removeWhenErr : (err : List Nat) -> (fields : List Nat) -> Maybe (List Nat)
      removeWhenErr [] fields = Just fields
      removeWhenErr _ _ = Nothing

main : IO ()
main = do
  inputData <- readFile "./data"
  case inputData of
       (Left err) => print err
       (Right inputData) => do
         let (tokens, debugLexed) = lex tokenMap inputData
             parsed = parse dataGrammar $ tok <$> tokens
         case parsed of
              Left (Error err _) => putStrLn err
              Right ((fieldConstraints, myTicket, nearbyTickets), debugParsed) => do
                let ticketErrors = countTicketErrors fieldConstraints <$> nearbyTickets
                    ans1 = sum $ concat ticketErrors
                --print ans1
                let safeNearbyTickets = removeWhenTicketError fieldConstraints nearbyTickets
                pure ()
         pure ()
  pure ()
