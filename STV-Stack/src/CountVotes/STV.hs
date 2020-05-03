module CountVotes.STV where

import CleanVotes.Clean
import Types

-- votesLength :: SortedVotes -> Int
-- votesLength votes = length votes

weight :: Double
weight = 1000

numSeats :: Int
numSeats = 4

quota :: SortedVotes -> Int
quota votes = ((length votes) `div` (numSeats + 1)) + 1

groupPref :: SortedVotes -> [String]
groupPref votes = map head votes

-- get tuple of candidate and vote (modified from https://codereview.stackexchange.com/questions/88720/return-list-with-numbers-of-color-occurrences-in-another-list)
votesRecieved :: Candidates -> [String] -> [VotesRecieved]
votesRecieved candidates xs = reverse (isort (zip candidates (map (\x -> length (filter (== x) xs)) candidates)))

removeCandidate :: [VotesRecieved] -> VotesRecieved -> [VotesRecieved]
removeCandidate allCans can = filter (/=can) allCans

-- steps for recursion
-- 1. Pass in canididate votes recieved
-- 2. Take head of that list and work out if quota is matched 
-- 3. If so, add to elected list. If greater than quota, distribute votes
-- 4. Else, drop the tail of the list and repeat 


-- WRONG WAY OF DOING IT, DOES NOT USE WEIGHTS

firstPref :: SortedVotes -> Candidates -> VotesRecieved
firstPref votes cans = head (votesRecieved cans (map head votes))

secondPref :: SortedVotes -> Candidates -> VotesRecieved
secondPref votes cans = head (votesRecieved cans (map head (filter (/=[]) (map (drop 1) votes))))

thirdPref :: SortedVotes -> Candidates -> VotesRecieved
thirdPref votes cans = head (votesRecieved cans (map head (filter (/=[]) (map (drop 2) votes))))

fourthPref :: SortedVotes -> Candidates -> VotesRecieved
fourthPref votes cans = head (votesRecieved cans (map head (filter (/=[]) (map (drop 3) votes))))

finalPrefs :: SortedVotes -> Candidates -> [VotesRecieved]
finalPrefs votes cans = [firstPref votes cans] ++ [secondPref votes cans] ++ [thirdPref votes cans] ++ [fourthPref votes cans]

elected :: [String]
elected = []

eliminated :: [String]
eliminated = []

runElection :: Int -> [String] -> [String] -> [VotesRecieved] -> [String]
runElection numSeats elected eliminated [] = elected
runElection 0 elected eliminated votingPrefs = elected
runElection numSeats elected eliminated votingPrefs = 
    if snd (head votingPrefs) > 50 then 
        runElection (numSeats - 1) (elected ++ [fst (head votingPrefs)]) (eliminated) (drop 1 (votingPrefs))
    else 
        runElection (numSeats) (elected) (eliminated ++ [fst (last votingPrefs)]) (removeCandidate votingPrefs (last votingPrefs))

startElection :: [VotesRecieved] -> [String]
startElection votingPrefs = runElection numSeats elected eliminated votingPrefs

