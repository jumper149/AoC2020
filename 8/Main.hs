{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Vector as V
import Text.Parsec

data Instruction =
    NOp Integer
  | Jmp Integer
  | Acc Accumulator
  deriving (Eq, Ord, Read, Show)

instructionParser :: Parsec String () [Instruction]
instructionParser = manyTill parseInstruction eof
  where parseInstruction = try parseNOp <|> try parseJmp <|> parseAcc
        parseNOp = do
          string "nop"
          n <- parseNumberAtEnd
          return $ NOp n
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

runInterpreter :: Interpreter a -> (a,Accumulator)
runInterpreter = runIdentity . flip evalStateT initState . runWriterT . unInterpreterT
  where initState = InterpreterState
          { currentLine = 0
          , visitedLines = []
          }

execInterpreter :: Interpreter a -> Accumulator
execInterpreter = snd . runInterpreter

newtype Program = Program { unProgram :: V.Vector Instruction }
  deriving (Eq, Ord, Read, Show)

constructProgram :: [Instruction] -> Program
constructProgram = Program . V.fromList

interpretInstruction :: Monad m => Instruction -> InterpreterT m ()
interpretInstruction (NOp _) = do
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

data ExitCode = ExitSuccess
              | ExitFailure
  deriving (Enum, Eq, Ord, Read, Show)

interpret' :: Monad m => Program -> InterpreterT m ExitCode
interpret' program = do
  InterpreterState {..} <- InterpreterT get
  if currentLine `elem` visitedLines
     then return ExitFailure
     else if currentLine == toEnum (V.length (unProgram program))
             then return ExitSuccess
             else do
               let currentInstruction = unProgram program V.! fromEnum currentLine
               interpretInstruction currentInstruction
               interpret' program

flipNOpJmp :: Instruction -> Instruction
flipNOpJmp (NOp n) = Jmp n
flipNOpJmp (Jmp n) = NOp n
flipNOpJmp (Acc n) = Acc n

changeLine :: (Instruction -> Instruction) -> Int -> Program -> Program
changeLine f i (Program instructions) = Program newInstructions
  where newInstruction = f $ instructions V.! i
        newInstructions = instructions V.// [(i,newInstruction)]

changeAllLines :: (Instruction -> Instruction) -> Program -> [Program]
changeAllLines f p = changeLine f <$> [0 .. pred lineCount] <*> pure p
  where lineCount = V.length $ unProgram p

main :: IO ()
main = do file <- readFile "./data"
          let instructionData = parse instructionParser "data" file
          case instructionData of
            Left err -> print err
            Right actualInstructionData -> do
              let program = constructProgram actualInstructionData
              print . unAccumulator . execInterpreter . interpret $ program
              let exits = fmap (runInterpreter . interpret') $ changeAllLines flipNOpJmp $ program
                  [(_,acc)] = filter ((== ExitSuccess) . fst) exits
              print $ unAccumulator acc
              return ()
