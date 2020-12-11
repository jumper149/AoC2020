module Main

removeDuplicatesFromSorted : Eq a => List a -> List a
removeDuplicatesFromSorted [] = []
removeDuplicatesFromSorted (x :: []) = [x]
removeDuplicatesFromSorted (x :: (y :: ys)) with (x == y)
  removeDuplicatesFromSorted (x :: (y :: ys)) | False = x :: removeDuplicatesFromSorted (y :: ys)
  removeDuplicatesFromSorted (x :: (y :: ys)) | True = removeDuplicatesFromSorted (y :: ys)

record Group where
  constructor MkGroup
  questions : List Char
  answers : List (List Char)

Show Group where
  show (MkGroup questions answers) = "Group " ++ show questions ++ " " ++ show answers

parseGroups : String -> List (List (List Char))
parseGroups = map (map unpack) . splitOn "" . lines

createGroup : List (List Char) -> Group
createGroup unsortedAnswers = MkGroup questions answers
  where answers = sort $ map sort unsortedAnswers
        questions = removeDuplicatesFromSorted . sort $ concat answers

countYesAnswers : Group -> Nat
countYesAnswers (MkGroup questions answers) = length questions

main : IO ()
main = do
  f <- readFile "./data"
  print $ (sum . map countYesAnswers . map createGroup . parseGroups) <$> f
