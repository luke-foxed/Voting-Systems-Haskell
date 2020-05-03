module CountVotes.STV where

import CleanVotes.Clean
-- votesLength :: [[String]] -> Int
-- votesLength votes = length votes

weight :: Double
weight = 1000

numSeats :: Int
numSeats = 4

quota :: [[String]] -> Int
quota votes = ((length votes) `div` (numSeats + 1)) + 1

groupPref :: [[String]] -> [String]
groupPref votes = map head votes

-- get tuple of candidate and vote (modified from https://codereview.stackexchange.com/questions/88720/return-list-with-numbers-of-color-occurrences-in-another-list)
votesRecieved :: [String] -> [String] -> [(String,Int)]
votesRecieved candidates xs = reverse (isort (zip candidates (map (\x -> length (filter (== x) xs)) candidates)))

removeCandidate :: [(String, Int)] -> (String, Int) -> [(String, Int)]
removeCandidate allCans can = filter (/=can) allCans

-- steps for recursion
-- 1. Pass in canididate votes recieved
-- 2. Take head of that list and work out if quota is matched 
-- 3. If so, add to elected list. If greater than quota, distribute votes
-- 4. Else, drop the tail of the list and repeat 


-- WRONG WAY OF DOING IT, DOES NOT USE WEIGHTS

firstPref :: [[String]] -> [String] -> (String, Int)
firstPref votes cans = head (votesRecieved cans (map head votes))

secondPref :: [[String]] -> [String] -> (String, Int)
secondPref votes cans = head (votesRecieved cans (map head (filter (/=[]) (map (drop 1) votes))))

thirdPref :: [[String]] -> [String] -> (String, Int)
thirdPref votes cans = head (votesRecieved cans (map head (filter (/=[]) (map (drop 2) votes))))

fourthPref :: [[String]] -> [String] -> (String, Int)
fourthPref votes cans = head (votesRecieved cans (map head (filter (/=[]) (map (drop 3) votes))))

finalPrefs :: [[String]] -> [String] -> [(String, Int)]
finalPrefs votes cans = [firstPref votes cans] ++ [secondPref votes cans] ++ [thirdPref votes cans] ++ [fourthPref votes cans]

elected :: [String]
elected = []

eliminated :: [String]
eliminated = []

runElection :: Int -> [String] -> [String] -> [(String, Int)] -> [String]
runElection numSeats elected eliminated [] = elected
runElection 0 elected eliminated votingPrefs = elected
runElection numSeats elected eliminated votingPrefs = 
    if snd (head votingPrefs) > 50 then 
        runElection (numSeats - 1) (elected ++ [fst (head votingPrefs)]) (eliminated) (drop 1 (votingPrefs))
    else 
        runElection (numSeats) (elected) (eliminated ++ [fst (last votingPrefs)]) (removeCandidate votingPrefs (last votingPrefs))

startElection :: [(String, Int)] -> [String]
startElection votingPrefs = runElection numSeats elected eliminated votingPrefs

