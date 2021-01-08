import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String

data Math = MathNum Integer
          | MathPlus Math Math
          | MathTimes Math Math
          | MathParens Math
          deriving (Eq, Ord, Read, Show)

dataParser :: Parser [Math]
dataParser = many (mathParser <* newline) <* eof
    where mathParser :: Parser Math
          mathParser = choice [ try plusParser
                              , try timesParser
                              , numParser
                              , parensParser
                              ]
          plusParser :: Parser Math
          plusParser = do
              x <- try parensParser <|> numParser
              void space
              void $ char '+'
              void space
              y <- mathParser
              let expr = MathPlus x y
              pure expr
          timesParser :: Parser Math
          timesParser = do
              x <- try parensParser <|> numParser
              void space
              void $ char '*'
              void space
              y <- mathParser
              let expr = MathTimes x y
              pure expr
          parensParser :: Parser Math
          parensParser = between (char '(') (char ')') mathParser
          numParser :: Parser Math
          numParser = MathNum . read <$> many1 digit

main :: IO ()
main = do
    inputData <- parseFromFile dataParser "./data"
    case inputData of
      Left err -> print err
      Right dat -> do
          print $ head dat
    return ()
