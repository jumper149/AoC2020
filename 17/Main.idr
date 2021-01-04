module Main

-- base
import Data.List
import Data.Strings
import System.File

-- contrib
import Text.Lexer
import Text.Lexer.Core
import Text.Parser
import Text.Parser.Core
import Text.Token

----------------------------------------------------------------------------------------------------

namespace Rules

  data Terminal = A
                | B

  Eq Terminal where
    A == A = True
    B == B = True
    _ == _ = False

  Show Terminal where
    show A = "A"
    show B = "A"

  data Ruleset : Type where
    Match : Terminal -> Ruleset
    References : List Nat -> Ruleset
    Option : Ruleset -> Ruleset -> Ruleset

  Show Ruleset where
    show (Match terminal) = "Match " ++ show terminal
    show (References ns) = "References " ++ show ns
    show (Option x y) = "Option " ++ show x ++ " " ++ show y

  record Rule where
    constructor MkRule
    number : Nat
    ruleset : Ruleset

  Show Rule where
    show (MkRule number ruleset) = "Rule " ++ show number ++ " " ++ show ruleset

----------------------------------------------------------------------------------------------------

namespace RulesParsing

  data RulesKind = RKNumber
                 | RKCharA
                 | RKCharB
                 | RKColon
                 | RKSpace
                 | RKPipe
                 | RKQuotationMarks

  Eq RulesKind where
    RKNumber == RKNumber = True
    RKCharA == RKCharA = True
    RKCharB == RKCharB = True
    RKColon == RKColon = True
    RKSpace == RKSpace = True
    RKPipe == RKPipe = True
    RKQuotationMarks == RKQuotationMarks = True
    _ == _ = False

  TokenKind RulesKind where
    TokType RKNumber = Nat
    TokType _ = ()
    tokValue RKNumber x = integerToNat $ cast x
    tokValue RKCharA _ = ()
    tokValue RKCharB _ = ()
    tokValue RKColon _ = ()
    tokValue RKSpace _ = ()
    tokValue RKPipe _ = ()
    tokValue RKQuotationMarks _ = ()

  tokenMap : TokenMap $ Token RulesKind
  tokenMap = toTokenMap
    [ (digits, RKNumber)
    , (is 'a', RKCharA)
    , (is 'b', RKCharB)
    , (is ':', RKColon)
    , (spaces, RKSpace)
    , (is '|', RKPipe)
    , (is '"', RKQuotationMarks)
    ]

  grammarRuleset : Grammar (Token RulesKind) True Ruleset
  grammarRuleset = grammarOption <|> grammarReferences <|> grammarMatch where
    grammarMatch : Grammar (Token RulesKind) True Ruleset
    grammarMatch = do
      match RKQuotationMarks
      terminal <- (match RKCharA *> pure A) <|> (match RKCharB *> pure B)
      match RKQuotationMarks
      pure $ Match terminal
    grammarReferences : Grammar (Token RulesKind) True Ruleset
    grammarReferences = References <$> sepBy1 (match RKSpace) (match RKNumber)
    grammarOption : Grammar (Token RulesKind) True Ruleset
    grammarOption = do
      left <- (grammarMatch <|> grammarReferences)
      match RKSpace
      match RKPipe
      match RKSpace
      right <- (grammarMatch <|> grammarReferences)
      pure $ Option left right

  grammarRule : Grammar (Token RulesKind) True Rule
  grammarRule = do
    number <- match RKNumber
    match RKColon
    match RKSpace
    ruleset <- grammarRuleset
    eof
    pure $ MkRule number ruleset

  export
  readRule : String -> Maybe Rule
  readRule str = do
    let (tokens, debugLexed) = lex tokenMap str
        parsed = parse grammarRule $ tok <$> tokens
    case parsed of
         Left _ => Nothing
         Right (rule, _) => Just rule

----------------------------------------------------------------------------------------------------

main : IO ()
main = do
  rulesData <- readFile "./rules"
  messagesData <- readFile "./messages"
  case (lines <$> rulesData, lines <$> messagesData) of
       (Right rulesData, Right messagesData) => do
         print $ readRule <$> rulesData
         pure ()
       _ => pure ()
  pure ()
