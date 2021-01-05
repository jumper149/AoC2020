module Main

-- base
import Data.List
import Data.Strings
import System.File

-- contrib
import Data.SortedMap
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
    show B = "B"

  data Ruleset : Type where
    Match : Terminal -> Ruleset
    References : (references : List Nat ** NonEmpty references) -> Ruleset
    Option : Ruleset -> Ruleset -> Ruleset

  Show Ruleset where
    show (Match terminal) = "Match " ++ show terminal
    show (References (ns**_)) = "References " ++ show ns
    show (Option x y) = "Option " ++ show x ++ " " ++ show y

  record Rule where
    constructor MkRule
    number : Nat
    ruleset : Ruleset

  [EqNumber] Eq Rule where
    (MkRule number _) == (MkRule number' _) = number == number'

  [OrdNumber] Ord Rule using EqNumber where
    compare (MkRule number _) (MkRule number' _) = compare number number'

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
    grammarReferences = References <$> sepBy1' (match RKSpace) (match RKNumber)
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

namespace ParserCombination

  ruleMap : List Rule -> SortedMap Nat Ruleset
  ruleMap rules = fromList $ extract <$> rules where
    extract : Rule -> (Nat, Ruleset)
    extract (MkRule number ruleset) = (number, ruleset)

  TestGrammar : Type
  TestGrammar = Grammar Terminal True ()

  TestGrammarMap : Type
  TestGrammarMap = SortedMap Nat TestGrammar

  lookupTestGrammar : TestGrammarMap -> Nat -> Maybe TestGrammar
  lookupTestGrammar = flip lookup

  mapOnDPair : (f : a -> b) -> (xs : List a ** NonEmpty xs) -> (ys : List b ** NonEmpty ys)
  mapOnDPair f ([x] ** IsNonEmpty) = ([f x] ** IsNonEmpty)
  mapOnDPair f ((x :: x' :: xs) ** IsNonEmpty) with (mapOnDPair f ((x' :: xs) ** IsNonEmpty))
    mapOnDPair f ((x :: x' :: xs) ** IsNonEmpty) | ((y :: ys) ** IsNonEmpty) = ((f x :: y :: ys) ** IsNonEmpty)

  lookupTestGrammars' : TestGrammarMap -> (references : List Nat ** NonEmpty references) ->
                                          (grammars : List (Maybe TestGrammar) ** NonEmpty grammars)
  lookupTestGrammars' map refs = mapOnDPair (lookupTestGrammar map) refs

  takeMaybeOutProof : (xs : List (Maybe a) ** NonEmpty xs) -> Maybe (ys : List a ** NonEmpty ys)
  takeMaybeOutProof ([ Nothing ] ** IsNonEmpty) = Nothing
  takeMaybeOutProof ([ Just x ] ** IsNonEmpty) = Just ([ x ] ** IsNonEmpty)
  takeMaybeOutProof ((Nothing :: x' :: xs) ** IsNonEmpty) = Nothing
  takeMaybeOutProof ((Just x :: x' :: xs) ** IsNonEmpty) with (takeMaybeOutProof ((x' :: xs) ** IsNonEmpty))
    takeMaybeOutProof ((Just x :: x' :: xs) ** IsNonEmpty) | Nothing = Nothing
    takeMaybeOutProof ((Just x :: x' :: xs) ** IsNonEmpty) | (Just dpair) = Just $ f x (g dpair) where
      f : a -> List a -> (bs : List a ** NonEmpty bs)
      f q qs = ((q :: qs) ** IsNonEmpty)
      g : (x : List a ** NonEmpty x) -> List a
      g (x ** y) = x

  lookupTestGrammars : TestGrammarMap -> (references : List Nat ** NonEmpty references) ->
                                         Maybe (grammars : List TestGrammar ** NonEmpty grammars)
  lookupTestGrammars map refs = takeMaybeOutProof $ lookupTestGrammars' map refs

  combineTestGrammars : TestGrammar -> TestGrammar -> TestGrammar
  combineTestGrammars x y = x *> y

  grammar : (ruleset : Ruleset) -> (grammars : TestGrammarMap) -> Maybe TestGrammar
  grammar (Match char) grammars =
    Just $ terminal "Failed to parse Terminal" test where
      test : Terminal -> Maybe ()
      test x = if x == char
                  then Just ()
                  else Nothing
  grammar (References references) grammars = do
    let testGrammarsStuff = lookupTestGrammars grammars references
    case testGrammarsStuff of
         Nothing => Nothing
         Just (testGrammars ** nonEmptyTestGrammarProof) =>
           Just $ foldl1 combineTestGrammars testGrammars
  grammar (Option leftRuleset rightRuleset) grammars =
    [| grammar leftRuleset grammars <|> grammar rightRuleset grammars |]

  grammarMap' : (rules : List Rule) -> TestGrammarMap -> TestGrammarMap
  grammarMap' [] acc = acc
  grammarMap' (rule@(MkRule number ruleset) :: rules) acc with (grammar ruleset acc)
    grammarMap' (rule@(MkRule number ruleset) :: rules) acc | Nothing = grammarMap' (rules ++ [rule]) acc
    grammarMap' (rule@(MkRule number ruleset) :: rules) acc | Just g = grammarMap' rules $ insert number g acc

  grammarMap : (rules : List Rule) -> TestGrammarMap
  grammarMap rules = grammarMap' rules empty

  export
  grammar0 : (rules : List Rule) -> Maybe TestGrammar
  grammar0 rules = lookup 0 $ grammarMap rules

  export
  testMessage : TestGrammar -> List Terminal -> Bool
  testMessage grammar word =
    case parse (grammar <* eof) word of
         Left _ => False
         Right _ => True

----------------------------------------------------------------------------------------------------

  newGrammar8 : TestGrammarMap -> Maybe TestGrammar
  newGrammar8 gmap = do
    grammar42 <- lookupTestGrammar gmap 42
    let g : TestGrammar
        g = do
          grammar42
          optional g
          pure ()
    Just $ g

  newGrammar11 : TestGrammarMap -> Maybe TestGrammar
  newGrammar11 gmap = do
    grammar42 <- lookupTestGrammar gmap 42
    grammar31 <- lookupTestGrammar gmap 31
    let g : TestGrammar
        g = do
          grammar42
          optional g
          grammar31
          pure ()
    Just g

  grammar' : (ruleset : Rule) -> (grammars : TestGrammarMap) -> Maybe TestGrammar
  grammar' (MkRule 8 _) grammars = newGrammar8 grammars
  grammar' (MkRule 11 _) grammars = newGrammar11 grammars
  grammar' (MkRule _ (Match char)) grammars =
    Just $ terminal "Failed to parse Terminal" test where
      test : Terminal -> Maybe ()
      test x = if x == char
                  then Just ()
                  else Nothing
  grammar' (MkRule _ (References references)) grammars = do
    let testGrammarsStuff = lookupTestGrammars grammars references
    case testGrammarsStuff of
         Nothing => Nothing
         Just (testGrammars ** nonEmptyTestGrammarProof) =>
           Just $ foldl1 combineTestGrammars testGrammars
  grammar' (MkRule _ (Option leftRuleset rightRuleset)) grammars =
    [| grammar' (MkRule 0 leftRuleset) grammars <|> grammar' (MkRule 0 rightRuleset) grammars |]

  grammar'Map' : (rules : List Rule) -> TestGrammarMap -> TestGrammarMap
  grammar'Map' [] acc = acc
  grammar'Map' (rule@(MkRule number ruleset) :: rules) acc with (grammar' rule acc)
    grammar'Map' (rule@(MkRule number ruleset) :: rules) acc | Nothing = grammar'Map' (rules ++ [rule]) acc
    grammar'Map' (rule@(MkRule number ruleset) :: rules) acc | Just g = grammar'Map' rules $ insert number g acc

  grammar'Map : (rules : List Rule) -> TestGrammarMap
  grammar'Map rules = grammar'Map' rules empty

  export
  grammar0' : (rules : List Rule) -> Maybe TestGrammar
  grammar0' rules = lookup 0 $ grammar'Map rules

namespace MessagesParsing

  toTerminal : Char -> Maybe Terminal
  toTerminal 'a' = Just A
  toTerminal 'b' = Just B
  toTerminal _ = Nothing

  export
  toTerminals : String -> Maybe $ List Terminal
  toTerminals message = traverse toTerminal $ unpack message

----------------------------------------------------------------------------------------------------

main : IO ()
main = do
  rulesData <- readFile "./rules"
  messagesData <- readFile "./messages"
  case (lines <$> rulesData, lines <$> messagesData) of
       (Right rulesData, Right messagesData) => do
         let mbRules = sort @{OrdNumber} <$> sequence (readRule <$> rulesData)
             mbMessages = traverse toTerminals messagesData
         case (mbRules, mbMessages) of
              (Just rules, Just messages) => do
                --case grammar0 rules of
                --     Nothing => putStrLn "Can't construct grammar0"
                --     Just grammar => do
                --       let tests = testMessage grammar <$> messages
                --           countParsed = length $ filter id tests
                --       print countParsed
                case grammar0' rules of
                     Nothing => putStrLn "Can't construct grammar0'"
                     Just grammar => do
                       let tests = testMessage grammar <$> messages
                           countParsed = length $ filter id tests
                       print countParsed
              _ => pure ()
         pure ()
       _ => pure ()
  pure ()
