module Main

-- base
import Text.Lexer
import Text.Lexer.Core
import Text.Parser
import Text.Parser.Core
import System.File

record FieldConstraint where
  constructor MkFieldConstraint
  field : String
  isValid : Nat -> Bool

Show FieldConstraint where
  show (MkFieldConstraint field isValid) =
    "FieldConstraint " ++ show field ++ " lambda"

data DataKind = DKString
              | DKNat
              | DKColon
              | DKMinus
              | DKOr
              | DKComma
              | DKNewline

Eq DataKind where
  DKString == DKString = True
  DKNat == DKNat = True
  DKColon == DKColon = True
  DKMinus == DKMinus = True
  DKOr == DKOr = True
  DKComma == DKComma = True
  DKNewline == DKNewline = True
  _ == _ = True

TokenKind DataKind where
  TokType DKString = String
  TokType DKNat = Nat
  TokType _ = ()
  tokValue DKString x = x
  tokValue DKNat x = integerToNat $ cast x
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
  , (digits, DKNat)
  , (some (alpha <+> is ' '), DKString)
  ]

fieldConstraintGrammar : Grammar (Token DataKind) True FieldConstraint
fieldConstraintGrammar = do
  field <- match DKString
  match DKColon
  xMin <- match DKNat
  match DKMinus
  xMax <- match DKNat
  match DKOr
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
  myTicket <- ticketGrammar
  match DKNewline
  nearbyTickets <- many ticketGrammar
  eof
  pure (fieldConstraints, myTicket, nearbyTickets)

--partial
--fieldConstraintParser : Parser FieldConstraint
--fieldConstraintParser = do
--  field <- takeWhile (/= ':')
--  --char ' '
--  xMin <- takeWhile (/= '-') >>= parse natParser
--  xMax <- takeWhile (/= ' ')
--  string "or "
--  yMin <- takeWhile (/= '-')
--  yMax <- takeWhile (/= '\n')
--  let constraint : Nat -> Bool
--      constraint n = n >= xMin && n <= xMax && n >= yMin && n <= yMax
--  pure $ MkFieldConstraint field $ const True -- constraint
--
--ticketParser : Parser $ List Nat
--
--dataParser : Parser ()
--dataParser = ?dataParserRhs

main : IO ()
main = do
  inputData <- readFile "./data"
  case inputData of
       (Left err) => print err
       (Right inputData) => do
         let (tokens, _) = lex tokenMap inputData
             parsed = parse dataGrammar $ tok <$> tokens
         print $ length tokens
         case parsed of
              Left (Error err _) => putStrLn err
              Right (sol, _) => do
                print sol
         pure ()
  pure ()
