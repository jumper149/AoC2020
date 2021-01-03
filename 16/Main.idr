module Main

-- base
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

wordsGrammar : Grammar (Token DataKind) False String
wordsGrammar = unwords <$> (match DKColon `sepBy` match DKWord) where

fieldConstraintGrammar : Grammar (Token DataKind) True FieldConstraint
fieldConstraintGrammar = do
  field <- wordsGrammar
  match DKColon
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
      constraint a = a >= xMin && a <= xMax && a >= yMin && a <= yMax
  pure $ MkFieldConstraint field constraint

ticketGrammar : Grammar (Token DataKind) True (List Nat)
ticketGrammar = do
  ns <- match DKComma `sepBy` match DKNat
  match DKNewline
  pure ns

dataGrammar : Grammar (Token DataKind) True (List FieldConstraint, List Nat, List $ List Nat)
dataGrammar = do
  fieldConstraints <- many fieldConstraintGrammar
  match DKNewline
  pure (fieldConstraints, [], [])
  --wordsGrammar
  --match DKColon
  --match DKNewline
  --myTicket <- ticketGrammar
  --match DKNewline
  --nearbyTickets <- many ticketGrammar
  --eof
  --pure (fieldConstraints, myTicket, nearbyTickets)

main : IO ()
main = do
  inputData <- readFile "./data"
  case inputData of
       (Left err) => print err
       (Right inputData) => do
         let (tokens, _) = lex tokenMap inputData
             parsed = parse dataGrammar $ tok <$> tokens
         print $ (text . tok) <$> tokens
         case parsed of
              Left (Error err _) => putStrLn err
              Right (sol, _) => do
                print sol
         pure ()
  pure ()
