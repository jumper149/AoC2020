module Main

-- base
import Data.Nat

record InputData where
  constructor MkInputData
  publicKeyCard : Nat
  publicKeyDoor : Nat

inputData : InputData
inputData = MkInputData 5290733 15231938

namespace Part1
  transformSubjectNumberStep : (subjectNumber : Integer) -> (value : Integer) -> Integer
  transformSubjectNumberStep subjectNumber value = (subjectNumber * value) `mod` 20201227

  export
  loopSize : (publicKeyCard : Nat) -> (subjectNumber : Nat) -> Nat
  loopSize publicKey subjectNumber = try 1 where
    publicKey' : Integer
    publicKey' = natToInteger publicKey
    subjectNumber' : Integer
    subjectNumber' = natToInteger subjectNumber
    try : (value : Integer) -> Nat
    try value = if publicKey' == value
                   then Z
                   else S $ try $ transformSubjectNumberStep subjectNumber' value

  transformSubjectNumber' : (loopSize : Nat) -> (subjectNumber : Integer) -> (value : Integer) -> Integer
  transformSubjectNumber' Z subjectNumber value = value
  transformSubjectNumber' (S k) subjectNumber value =
    transformSubjectNumber' k subjectNumber $ transformSubjectNumberStep subjectNumber value

  export
  transformSubjectNumber : (loopSize : Nat) -> (subjectNumber : Nat) -> Nat
  transformSubjectNumber loopSize subjectNumber =
    integerToNat $ transformSubjectNumber' loopSize (natToInteger subjectNumber) 1

loopSizeCard : Nat
loopSizeCard = loopSize inputData.publicKeyCard 7

encryptionKey1 : Nat
encryptionKey1 = transformSubjectNumber loopSizeCard inputData.publicKeyDoor

loopSizeDoor : Nat
loopSizeDoor = loopSize inputData.publicKeyDoor 7

encryptionKey2 : Nat
encryptionKey2 = transformSubjectNumber loopSizeDoor inputData.publicKeyCard

main : IO ()
main = do
  putStrLn . show $ encryptionKey1
  putStrLn . show $ encryptionKey2
  pure ()
