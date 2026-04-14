module Main where

import Ex5

-- Q3
-- T1
-- classify numbers by factors
factors :: [Int]
factors = [5, 391, 85, 17, 23, 115]

-- find which factor group a number belongs to
classifyNumber :: Int -> Maybe Int
classifyNumber n = findFactor factors
  where
    findFactor [] = Nothing
    findFactor (f : fs)
      | n `mod` f == 0 = Just f
      | otherwise = findFactor fs

-- group numbers
classifyAll :: [Int] -> [(Int, [Int])]
classifyAll nums =
  let grouped = [(f, [n | n <- nums, classifyNumber n == Just f]) | f <- factors]
      multiplesOf1 = [n | n <- nums, classifyNumber n == Nothing]
   in grouped ++ [(1, multiplesOf1)]

-- process numbers.txt, create classified.txt
task1 :: IO ()
task1 = do
  contents <- readFile "numbers.txt"
  let numbers = read contents :: [Int]
  let classified = classifyAll numbers
  writeFile "classified.txt" (unlines [show pair | pair <- classified])

-- T2
-- element at index i from a list - Nothing if out of bounds
safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x : xs) 0 = Just x
safeIndex (x : xs) i = safeIndex xs (i - 1)

-- find max len of lists
maxLength :: [[a]] -> Int
maxLength [] = 0
maxLength lists = maximum (map length lists)

-- fault tolerance
ftTranspose :: [[Int]] -> [[Maybe Int]]
ftTranspose lists =
  let maxLen = maxLength lists
   in [[safeIndex lst i | lst <- lists] | i <- [0 .. maxLen - 1]]

-- process numlists.txt and create ft_transpose.txt
task2 :: IO ()
task2 = do
  contents <- readFile "numlists.txt"
  let numlists = read contents :: [[Int]]
  let transposed = ftTranspose numlists
  writeFile "ft_transpose.txt" (show transposed ++ "\n")

runQ3 :: IO ()
runQ3 = do
  task1
  task2

main :: IO ()
main = do
  putStrLn "Running Exercise5."
  putStrLn ""
  runQ3
  putStrLn "You should modify this program as described in Ex5.hs Q3"
  putStrLn "Remember to submit Ex5.rns, Ex5.hs and Main.hs (this program)."
