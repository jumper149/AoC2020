module Main

-- base
import Control.Monad.State
import Data.List
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

Resolution1D : Nat
Resolution1D = 10

Tiling : Type
Tiling = Vect Resolution1D $ Vect Resolution1D Bool

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

-- from square root of `length tiles`
Resolution1DImage : Nat
Resolution1DImage = 12

Arrangement : Type
Arrangement = Vect Resolution1DImage $ Vect Resolution1DImage $ Maybe Nat

nextCoordinate : {n : Nat} -> (x : Fin $ S n) -> Fin $ S n
nextCoordinate x with (strengthen x)
  nextCoordinate x | (Left y) = FZ
  nextCoordinate x | (Right y) = FS y

nextCoordinates : {n : Nat} ->
                  (x : Fin n) ->
                  (y : Fin n) ->
                  Maybe (Fin n, Fin n)
nextCoordinates x y with (n)
  nextCoordinates x y | Z = Nothing
  nextCoordinates x y | (S k) =
    if x == last
       then if y == last
               then Nothing
               else Just (FZ, nextCoordinate y)
       else Just (nextCoordinate x, nextCoordinate y)

data Direction = DirectionLeft
               | DirectionUp

neighborsToCheck : {n : Nat} ->
                   (x : Fin n) ->
                   (y : Fin n) ->
                   List ((Fin n, Fin n), Direction)
neighborsToCheck x y = [ ((pred x, y), DirectionUp), ((x, pred y), DirectionLeft) ] where
  pred : {n : Nat} -> (x : Fin n) -> Fin n
  pred FZ = last -- this is additional and not necessary, but allows returning Vect
  pred (FS k) = weaken k

neighborsToCheck' : (arrangement : Arrangement) ->
                    (x : Fin Resolution1DImage) ->
                    (y : Fin Resolution1DImage) ->
                    List (Nat, Direction)
neighborsToCheck' arrangement x y = do
  ((x', y'), d') <- neighborsToCheck x y
  let n = index x' $ index y' arrangement
  case n of
       Nothing => []
       Just n' => pure (n', d')

checkTiling : Direction -> (new : Tiling) -> (old : Tiling) -> Bool
checkTiling DirectionLeft new old = (index last <$> old) == (index FZ <$> new)
checkTiling DirectionUp new old = index last old == index FZ new

checkNeighbors : (tilingMap : TilingMap) ->
                 (new : Tiling) ->
                 List (Nat, Direction) ->
                 Bool
checkNeighbors tilingMap new [] = True
checkNeighbors tilingMap new ((n, d) :: xs) =
  case lookup n tilingMap of
       Nothing => ?keysShouldMatch
       Just old => if checkTiling d new old
                      then checkNeighbors tilingMap new xs
                      else False

checkNeighbors' : (tilingMap : TilingMap) ->
                  (arrangement : Arrangement) ->
                  (new : Tiling) ->
                  (x : Fin Resolution1DImage) ->
                  (y : Fin Resolution1DImage) ->
                  Bool
checkNeighbors' tilingMap arrangement new x y =
  checkNeighbors tilingMap new $ neighborsToCheck' arrangement x y

filterTilings : (tilingMap : TilingMap) ->
                (arrangement : Arrangement) ->
                (tilings : List (Nat, Tiling)) ->
                (x : Fin Resolution1DImage) ->
                (y : Fin Resolution1DImage) ->
                List (Nat, Tiling)
filterTilings tilingMap arrangement tilings x y =
  filter (\(_,t) => checkNeighbors' tilingMap arrangement t x y) tilings

tryTilings : (tilingMap : TilingMap) ->
             (arrangement : Arrangement) ->
             (x : Fin Resolution1DImage) ->
             (y : Fin Resolution1DImage) ->
             List Arrangement
tryTilings tilingMap arrangement x y = ?tryTilings_rhs where
  tilingsToTry : List (Nat, Tiling)
  tilingsToTry = do
    (n, t) <- Data.SortedMap.toList tilingMap
    t' <- toList $ tilingOrientations t
    pure (n, t')
  possibleTilings : List (Nat, Tiling)
  possibleTilings = filterTilings tilingMap arrangement tilingsToTry x y
  nextTry : (Nat, Tiling) -> List Arrangement
  nextTry (n, t) = case nextCoordinates x y of
                        Nothing => pure newArrangement
                        Just (x',y') => tryTilings newTilingMap newArrangement x' y'
                        where
                          newTilingMap : TilingMap
                          newTilingMap = insert n t tilingMap
                          newArrangement : Arrangement
                          newArrangement = replaceAt y (replaceAt x (Just n) $ index y arrangement) arrangement

findTilings : (tilingMap : TilingMap) ->
              List Arrangement
findTilings tilingMap = tryTilings tilingMap (pure $ pure Nothing) FZ FZ

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
      countExactly Resolution1D $ countExactly Resolution1D grammarPixel <* match DKNewline
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
                print $ findTilings tilingMap
  pure ()
