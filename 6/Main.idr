module Main

removeDuplicatesFromSorted : Eq a => List a -> List a
removeDuplicatesFromSorted [] = []
removeDuplicatesFromSorted (x :: []) = [x]
removeDuplicatesFromSorted (x :: (y :: ys)) with (x == y)
  removeDuplicatesFromSorted (x :: (y :: ys)) | False = x :: removeDuplicatesFromSorted (y :: ys)
  removeDuplicatesFromSorted (x :: (y :: ys)) | True = removeDuplicatesFromSorted (y :: ys)

elemInAll : Eq a => a -> List (List a) -> Bool
elemInAll x xss = all (elem x) xss

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

countEveryoneYesAnswers : Group -> Nat
countEveryoneYesAnswers (MkGroup questions answers) =
  length $ filter id $ elemInAll <$> questions <*> pure answers

main : IO ()
main = do
  f <- readFile "./data"

  putStrLn "Part 1"
  print $ (sum . map countYesAnswers . map createGroup . parseGroups) <$> f
  putStrLn ""

  putStrLn "Part 2"
  print $ (sum . map countEveryoneYesAnswers . map createGroup . parseGroups) <$> f
  putStrLn ""
