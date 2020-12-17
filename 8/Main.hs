{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Vector as V
import Text.Parsec

data Instruction =
    NOp
  | Jmp Integer
  | Acc Accumulator
  deriving (Eq, Ord, Read, Show)

instructionParser :: Parsec String () [Instruction]
instructionParser = manyTill parseInstruction eof
  where parseInstruction = try parseNOp <|> try parseJmp <|> parseAcc
        parseNOp = do
          string "nop"
          parseNumberAtEnd
          return NOp
        parseJmp = do
          string "jmp"
          n <- parseNumberAtEnd
          return $ Jmp n
        parseAcc = do
          string "acc"
          n <- parseNumberAtEnd
          return $ Acc $ Accumulator n
        parseNumberAtEnd = do
          char ' '
          sign <- oneOf "+-"
          let factor = case sign of
                         '+' -> 1
                         '-' -> -1
                         _ -> undefined
          n <- read <$> many1 digit
          newline
          return $ factor * n

newtype Accumulator = Accumulator { unAccumulator :: Integer }
  deriving (Enum, Eq, Ord, Read, Show)

instance Semigroup Accumulator where
  (<>) (Accumulator x) (Accumulator y) = Accumulator $ x + y

instance Monoid Accumulator where
  mempty = Accumulator 0

data InterpreterState = InterpreterState
  { currentLine :: Integer
  , visitedLines :: [Integer]
  }
  deriving (Eq, Ord, Read, Show)

newtype InterpreterT m a = InterpreterT { unInterpreterT :: WriterT Accumulator (StateT InterpreterState m) a }
  deriving (Functor, Applicative, Monad)

type Interpreter = InterpreterT Identity

runInterpreter :: Interpreter () -> Accumulator
runInterpreter = runIdentity . flip evalStateT initState . execWriterT . unInterpreterT
  where initState = InterpreterState
          { currentLine = 0
          , visitedLines = []
          }

newtype Program = Program { unProgram :: V.Vector Instruction }
  deriving (Eq, Ord, Read, Show)

constructProgram :: [Instruction] -> Program
constructProgram = Program . V.fromList

interpretInstruction :: Monad m => Instruction -> InterpreterT m ()
interpretInstruction NOp = do
  InterpreterState {..} <- InterpreterT get
  let visitedLines' = currentLine : visitedLines
      currentLine' = succ currentLine
  InterpreterT $ put InterpreterState {currentLine = currentLine', visitedLines = visitedLines'}
  return ()
interpretInstruction (Jmp destination) = do
  InterpreterState {..} <- InterpreterT get
  let visitedLines' = currentLine : visitedLines
      currentLine' = currentLine + destination
  InterpreterT $ put InterpreterState {currentLine = currentLine', visitedLines = visitedLines'}
  return ()
interpretInstruction (Acc accumulator) = do
  InterpreterState {..} <- InterpreterT get
  InterpreterT $ tell accumulator
  let visitedLines' = currentLine : visitedLines
      currentLine' = succ currentLine
  InterpreterT $ put InterpreterState {currentLine = currentLine', visitedLines = visitedLines'}
  return ()

interpret :: Monad m => Program -> InterpreterT m ()
interpret program = do
  InterpreterState {..} <- InterpreterT get
  if currentLine `elem` visitedLines
     then return ()
     else do
       let currentInstruction = unProgram program V.! fromEnum currentLine
       interpretInstruction currentInstruction
       interpret program

main :: IO ()
main = do file <- readFile "./data"
          let instructionData = parse instructionParser "data" file
          case instructionData of
            Left err -> print err
            Right actualInstructionData -> do
              print . unAccumulator . runInterpreter . interpret . constructProgram $ actualInstructionData
          return ()
