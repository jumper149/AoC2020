import Control.Monad
import Control.Monad.Identity
import Data.Functor
import Text.Parsec
import Text.Parsec.String

data Operation = Plus
               | Times
               deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Math = MathNum Integer
          | MathOp Operation Math Math
          | MathParens Math
          deriving (Eq, Ord, Read, Show)

data ReversedMath = RMathNum Integer
                  | RMathOp Operation ReversedMath ReversedMath
                  | RMathParens ReversedMath
                  deriving (Eq, Ord, Read, Show)

dataParser :: Parser [Math]
dataParser = many (mathParser <* newline) <* eof
    where mathParser :: Parser Math
          mathParser = choice [ try opParser
                              , numParser
                              , parensParser
                              ]
          opParser :: Parser Math
          opParser = do
              x <- try parensParser <|> numParser
              void space
              op <- char '+' $> Plus <|> char '*' $> Times
              void space
              y <- mathParser
              let expr = MathOp op x y
              pure expr
          parensParser :: Parser Math
          parensParser = between (char ')') (char '(') (MathParens <$> mathParser)
          numParser :: Parser Math
          numParser = MathNum . read <$> many1 digit

evalOp :: Operation -> (Integer -> Integer -> Integer)
evalOp Plus = (+)
evalOp Times = (*)

calculate :: Math -> Integer
calculate (MathNum i) = i
calculate (MathOp op x y) = evalOp op (calculate x) (calculate y)
calculate (MathParens m) = calculate m

data MathToken = TokenMathNum Integer
               | TokenMathParensOpen
               | TokenMathParensClose
               | TokenMathOp Operation
               deriving (Eq, Ord, Read, Show)

tokenize :: Math -> [MathToken]
tokenize (MathNum i) = [ TokenMathNum i ]
tokenize (MathParens m) = TokenMathParensOpen : tokenize m ++ [ TokenMathParensClose ]
tokenize (MathOp op x y) = tokenize x ++ [ TokenMathOp op ] ++ tokenize y

type PrecedenceParser a = ParsecT [MathToken] () Identity a

precedenceParser :: PrecedenceParser Math
precedenceParser = timesParser
    where mathToken :: (MathToken -> Maybe a) -> PrecedenceParser a
          mathToken f = do currentPos <- getPosition
                           token show (const currentPos) f
          foldOp :: Operation -> [Math] -> Math
          foldOp _ [x] = x
          foldOp op (x:y:z) = MathOp op x $ foldOp op $ y : z
          foldOp _ _ = undefined
          timesParser :: PrecedenceParser Math
          timesParser = foldOp Times <$> (try plusParser <|> parensParser) `sepBy1` mathToken isTimes
              where isTimes (TokenMathOp Times) = Just ()
                    isTimes _ = Nothing
          plusParser :: PrecedenceParser Math
          plusParser = foldOp Plus <$> (try numParser <|> parensParser) `sepBy1` mathToken isPlus
              where isPlus (TokenMathOp Plus) = Just ()
                    isPlus _ = Nothing
          parensParser :: PrecedenceParser Math
          parensParser = do
              mathToken isOpen
              m <- timesParser
              mathToken isClose
              pure m
                  where isOpen TokenMathParensOpen = Just ()
                        isOpen _ = Nothing
                        isClose TokenMathParensClose = Just ()
                        isClose _ = Nothing
          numParser :: PrecedenceParser Math
          numParser = mathToken isNum
              where isNum (TokenMathNum i) = Just $ MathNum i
                    isNum _ = Nothing

main :: IO ()
main = do
    fileData <- readFile "./data"
    let reversedFileData = unlines . (reverse <$>) . lines $ fileData
        reversedInputData = parse dataParser "data-reverse" reversedFileData
    case reversedInputData of
      Left err -> print err
      Right dat -> do
          let sol1 = sum $ calculate <$> dat
          print sol1
          let precedenceMath = traverse (parse precedenceParser "math-precedence" . tokenize) dat
          case precedenceMath of
            Left err' -> print err'
            Right dat' -> do
                let sol2 = sum $ calculate <$> dat'
                print sol2
    return ()
