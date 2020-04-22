module CleanVotes.Clean where

import Data.List.Split (splitOn)

weight :: Double
weight = 1000

numSeats = 4

-- convert string output to nested list of votes
parseRawVotes :: String -> [[String]]
parseRawVotes rawVotes = map convertToList (tail (splitOn "\n" rawVotes))

-- convert each newline string to a list
convertToList :: String -> [String]
convertToList vote = splitOn "," vote

-- get candidates
candidates :: [[String]] -> [String]
candidates votes = drop 2 (head votes)

-- ignore list items that contain empty strings
rmEmptyStrings :: [[String]] -> [[String]]
rmEmptyStrings = filter (not . null) . map (filter (not . null)) 

-- keep only the voting scores from each list
rmNames :: [[String]] -> [[String]]
rmNames (x:xs) = [ drop 2 x | x <- xs]

cleanVotes :: [[String]] -> [[String]]
cleanVotes votes = rmNames (rmEmptyStrings votes)

votesLength :: [[String]] -> Int
votesLength votes = length (cleanVotes votes)

quota :: [[String]] -> Int
quota votes = ((votesLength votes) `div` (numSeats + 1)) + 1

-- go through each vote and apply zipCandidate
groupCandidateVotes :: [[String]] -> [[(String, String)]]
groupCandidateVotes votes = map (zipCandidate votes) (cleanVotes votes)

-- zip candidate with specific vote
zipCandidate :: [[String]] -> [String] -> [(String, String)]
zipCandidate votes = zip (candidates votes)

sortVotes :: [[String]] -> [[(String, String)]]
sortVotes votes = map isort (groupCandidateVotes votes)

-- taken from notes 
isort :: Ord a => [(String, a)] -> [(String, a)]
isort [] = []
isort (x:xs) = insertion x (isort xs)

-- taken from notes and modified to apply on second string in tuple
insertion :: Ord a => (String, a) -> [(String, a)]  -> [(String, a)]
insertion x [] = [x]
insertion x (y:ys) 
                | snd x <= snd y = x : y : ys
                | otherwise = y: insertion x ys

-- return list of candidates sorted by vote number
extractVotes :: [[String]] -> [[String]]
extractVotes votes = map rmEmptyVotes (sortVotes votes)

-- remove asterix and vote number which isn't needed 
rmEmptyVotes :: [(String, String)] -> [String]
rmEmptyVotes vote = [fst x | x <- vote, snd x /= "*"]
