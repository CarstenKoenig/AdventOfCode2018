module Day1.Solution where


run :: IO ()
run = do
  putStrLn "day1"
  myInput <- input
  putStrLn $ "star 1: " ++ show (solution myInput)


type Input = [Int]
type Output = Int


input :: IO Input
input = map parseNr . lines <$> readFile "./src/Day1/input.txt"
  where
    parseNr ('-':n) = negate $ read n
    parseNr ('+':n) = read n


solution :: Input -> Output
solution = sum
