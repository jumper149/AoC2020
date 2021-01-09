import Control.Monad
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

main :: IO ()
main = do
    fileData <- readFile "./data"
    let reversedFileData = unlines . (reverse <$>) . lines $ fileData
        reversedInputData = parse dataParser "data" reversedFileData
    case reversedInputData of
      Left err -> print err
      Right dat -> do
          let sol1 = sum $ calculate <$> dat
          print sol1
    return ()
