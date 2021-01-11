module Main

-- base
import Data.Nat
import Data.Vect
import System.File

-- contrib
import Data.SortedMap
import Text.Lexer
import Text.Lexer.Core
import Text.Parser
import Text.Parser.Core
import Text.Token

----------------------------------------------------------------------------------------------------

resolution1D : Nat
resolution1D = 10

Tiling : Type
Tiling = Vect resolution1D $ Vect resolution1D Bool

tilingOrientations : Tiling -> Vect 16 Tiling
tilingOrientations tiling = concat $ tilingRotations <$> tilingFlippings tiling where
  tilingFlippings : Tiling -> Vect 4 Tiling
  tilingFlippings tiling =
    [ tiling
    , reverse tiling
    , reverse <$> tiling
    , reverse <$> reverse tiling
    ]
  tilingRotations : Tiling -> Vect 4 Tiling
  tilingRotations tiling =
    [ tiling
    , rotate90 tiling
    , rotate90 $ rotate90 tiling
    , rotate90 $ rotate90 $ rotate90 tiling
    ] where
      rotate90 : Tiling -> Tiling
      rotate90 = reverse . transpose

record Tile where
  constructor MkTile
  id : Nat
  tiling : Tiling

Show Tile where
  show (MkTile id tiling) = "Tile " ++ show id ++ " " ++ show tiling

TilingMap : Type
TilingMap = SortedMap Nat Tiling

createTilingMap : List Tile -> TilingMap
createTilingMap = fromList . map f where
  f : Tile -> (Nat, Tiling)
  f (MkTile id tiling) = (id, tiling)

Arrangement : Nat -> Type
Arrangement n = Vect n $ Vect n $ Maybe Nat

----------------------------------------------------------------------------------------------------

namespace Parsing

  data DataKind = DKTile
                | DKSpace
                | DKNum
                | DKColon
                | DKNewline
                | DKDot
                | DKHash

  Eq DataKind where
    DKTile == DKTile = True
    DKSpace == DKSpace = True
    DKNum == DKNum = True
    DKColon == DKColon = True
    DKNewline == DKNewline = True
    DKDot == DKDot = True
    DKHash == DKHash = True
    _ == _ = False

  TokenKind DataKind where
    TokType DKNum = Nat
    TokType _ = ()
    tokValue DKNum x = integerToNat $ cast x
    tokValue DKTile _ = ()
    tokValue DKSpace _ = ()
    tokValue DKColon _ = ()
    tokValue DKNewline _ = ()
    tokValue DKDot _ = ()
    tokValue DKHash _ = ()

  tokenMap : TokenMap $ Token DataKind
  tokenMap = toTokenMap
    [ (digits, DKNum)
    , (is '.', DKDot)
    , (is '#', DKHash)
    , (exact "Tile", DKTile)
    , (newline, DKNewline)
    , (space, DKSpace)
    , (is ':', DKColon)
    ]

  countExactly : (n : Nat) -> (p : Grammar tok True a) -> Grammar tok (isSucc n) (Vect n a)
  countExactly Z p = Empty []
  countExactly (S k) p = [| p :: countExactly k p |]

  grammarTiles : Grammar (Token DataKind) True (List Tile)
  grammarTiles = sepBy1 (match DKNewline) grammarTile where
    grammarPixel : Grammar (Token DataKind) True Bool
    grammarPixel = match DKDot $> False <|> match DKHash $> True
    grammarTiling : Grammar (Token DataKind) True Tiling
    grammarTiling =
      countExactly resolution1D $ countExactly resolution1D grammarPixel <* match DKNewline
    grammarTile : Grammar (Token DataKind) True Tile
    grammarTile = do
      match DKTile
      match DKSpace
      id <- match DKNum
      match DKColon
      match DKNewline
      tiling <- grammarTiling
      pure $ MkTile id tiling

  export
  readData : String -> Maybe $ List Tile
  readData str = do
    let (tokens, debugLexed) = lex tokenMap str
        parsed = parse grammarTiles $ tok <$> tokens
    case parsed of
         Left _ => Nothing
         Right (rule, _) => Just rule

----------------------------------------------------------------------------------------------------

main : IO ()
main = do
  inputData <- readFile "./data"
  case inputData of
       Left err => print err
       Right dat => do
         case readData dat of
              Nothing => pure ()
              Just tiles => do
                let tilingMap = createTilingMap tiles
                print tilingMap
  pure ()
