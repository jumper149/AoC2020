module Main

-- base
import Data.List
import System.File

record InputData where
  constructor MkInputData
  publicKeyCard : Nat
  publicKeyDoor : Nat

inputData : InputData
inputData = MkInputData 5290733 15231938

--handshake

transformSubjectNumber : (loopSize : Nat) -> (subjectNumber : Nat) -> Nat

transformSubjectNumberStep : (subjectNumber : Nat) -> (value : Nat) -> Nat

main : IO ()
main = do
  pure ()
